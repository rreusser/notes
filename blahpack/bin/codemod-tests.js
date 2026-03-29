#!/usr/bin/env node

// Codemod for test files — applies mechanical stdlib convention fixes.
//
// Usage:
//   node bin/codemod-tests.js lib/blas/base/dcopy/test/test.js
//   node bin/codemod-tests.js --dry-run lib/blas/base/{name}/test/test.js
//
// Fixes applied:
//   1. Remove stdlib/require-globals from eslint-disable (we require globals)
//   2. Add missing @stdlib requires for Float64Array etc.
//   3. Replace Array.from() with toArray() helper + add the helper
//   4. Within each test() callback, hoist var declarations to the top
//      (fixes no-use-before-define) and sort by length (stdlib/vars-order)
//   5. Add eslint-disable-line max-len to long lines
//   6. Add eslint-disable-line node/no-sync to readFileSync

'use strict';

var esprima = require( 'esprima' );
var escodegen = require( 'escodegen' );
var fs = require( 'fs' );

var dryRun = false;
var files = [];
var i;

for ( i = 2; i < process.argv.length; i++ ) {
	if ( process.argv[ i ] === '--dry-run' ) {
		dryRun = true;
	} else {
		files.push( process.argv[ i ] );
	}
}

if ( files.length === 0 ) {
	process.stderr.write(
		'Usage: node bin/codemod-tests.js [--dry-run] <files...>\n'
	);
	process.exit( 1 );
}

var GLOBAL_REQUIRES = {
	'Float64Array': '@stdlib/array/float64',
	'Float32Array': '@stdlib/array/float32',
	'Int32Array': '@stdlib/array/int32',
	'Complex128Array': '@stdlib/array/complex128',
	'Complex64Array': '@stdlib/array/complex64'
};

var TO_ARRAY_HELPER =
	'/**\n' +
	'* Converts a typed array to a plain array.\n' +
	'*\n' +
	'* @private\n' +
	'* @param {TypedArray} arr - input array\n' +
	'* @returns {Array} output array\n' +
	'*/\n' +
	'function toArray( arr ) {\n' +
	'\tvar out = [];\n' +
	'\tvar i;\n' +
	'\tfor ( i = 0; i < arr.length; i++ ) {\n' +
	'\t\tout.push( arr[ i ] );\n' +
	'\t}\n' +
	'\treturn out;\n' +
	'}';

function processFile( file ) {
	if ( !fs.existsSync( file ) ) {
		return false;
	}
	var src = fs.readFileSync( file, 'utf8' );
	var original = src;

	// --- Phase 1: Text-level transforms ---

	// 1a. Remove stdlib/require-globals from eslint-disable
	src = src.replace( /,\s*stdlib\/require-globals/g, '' );
	src = src.replace( /stdlib\/require-globals,\s*/g, '' );
	src = src.replace( /stdlib\/require-globals/g, '' );
	src = src.replace( /\/\*\s*eslint-disable\s*,\s*/g, '/* eslint-disable ' );
	src = src.replace( /,\s*\*\//g, ' */' );

	// 1b. Add missing @stdlib requires for globals
	var globalNames = Object.keys( GLOBAL_REQUIRES );
	var addRequires = [];
	globalNames.forEach( function checkGlobal( name ) {
		var useRE = new RegExp( '\\bnew\\s+' + name + '\\b' );
		var reqRE = new RegExp( 'var\\s+' + name + '\\s*=\\s*require' );
		if ( useRE.test( src ) && !reqRE.test( src ) ) {
			addRequires.push(
				'var ' + name + ' = require( \'' +
				GLOBAL_REQUIRES[ name ] + '\' );'
			);
		}
	});
	if ( addRequires.length > 0 ) {
		var lastReq = src.lastIndexOf( 'require(' );
		if ( lastReq !== -1 ) {
			var lineEnd = src.indexOf( '\n', lastReq );
			if ( lineEnd !== -1 ) {
				src = src.substring( 0, lineEnd + 1 ) +
					addRequires.join( '\n' ) + '\n' +
					src.substring( lineEnd + 1 );
			}
		}
	}

	// 1c. Replace Array.from(x) with toArray( x )
	var hadArrayFrom = /Array\.from\s*\(/.test( src );
	src = src.replace( /Array\.from\(\s*/g, 'toArray( ' );

	// 1d. Add toArray helper if needed and not present
	if ( hadArrayFrom && !/function toArray/.test( src ) ) {
		var testsMarker = src.indexOf( '// TESTS //' );
		var firstTest = src.indexOf( "test( '" );
		var insertBefore = ( testsMarker !== -1 ) ? testsMarker : firstTest;

		if ( insertBefore !== -1 ) {
			var hasFuncSection = /\/\/ FUNCTIONS \/\//.test( src );
			var block;
			if ( !hasFuncSection ) {
				block = '\n// FUNCTIONS //\n\n' + TO_ARRAY_HELPER + '\n\n\n';
			} else {
				block = TO_ARRAY_HELPER + '\n\n';
			}
			src = src.substring( 0, insertBefore ) + block +
				src.substring( insertBefore );
		}
	}

	// --- Phase 2: AST-level var hoisting in test callbacks ---

	var ast;
	try {
		ast = esprima.parseScript( src, { range: true, loc: true } );
	} catch ( e ) {
		process.stderr.write( 'Parse error in ' + file + ': ' + e.message + '\n' );
		return applyTextFixes( src, original, file );
	}

	// Find all test() call expressions with FunctionExpression callbacks
	var edits = [];

	function walk( node ) {
		if ( !node || typeof node !== 'object' ) {
			return;
		}
		if (
			node.type === 'ExpressionStatement' &&
			node.expression.type === 'CallExpression' &&
			node.expression.callee.type === 'Identifier' &&
			node.expression.callee.name === 'test' &&
			node.expression.arguments.length >= 2
		) {
			var fn = node.expression.arguments[ 1 ];
			if (
				fn.type === 'FunctionExpression' &&
				fn.body.type === 'BlockStatement'
			) {
				hoistVarsInBody( fn.body, edits );
			}
		}
		var keys = Object.keys( node );
		var j;
		var child;
		for ( j = 0; j < keys.length; j++ ) {
			child = node[ keys[j] ];
			if ( Array.isArray( child ) ) {
				child.forEach( walk );
			} else if ( child && typeof child === 'object' && child.type ) {
				walk( child );
			}
		}
	}

	function hoistVarsInBody( body, edits ) {
		var stmts = body.body;
		if ( !stmts || stmts.length === 0 ) {
			return;
		}

		// Collect all var declarations in this body
		var varDecls = [];
		var hasOutOfOrderVars = false;
		var seenNonVar = false;
		var j;
		var k;
		var stmt;

		for ( j = 0; j < stmts.length; j++ ) {
			stmt = stmts[ j ];
			if ( stmt.type === 'VariableDeclaration' ) {
				for ( k = 0; k < stmt.declarations.length; k++ ) {
					var d = stmt.declarations[ k ];
					varDecls.push({
						name: d.id.name,
						hasInit: d.init !== null,
						initSrc: d.init ?
							src.substring( d.init.range[0], d.init.range[1] ) :
							null,
						stmtRange: stmt.range,
						stmtIdx: j,
						afterNonVar: seenNonVar
					});
				}
				if ( seenNonVar ) {
					hasOutOfOrderVars = true;
				}
			} else {
				seenNonVar = true;
			}
		}

		if ( varDecls.length === 0 ) {
			return;
		}

		// Check if any var initializer references a later-declared var
		var varNames = {};
		varDecls.forEach( function( v ) { varNames[ v.name ] = true; });

		var hasForwardRef = false;
		varDecls.forEach( function checkForward( v ) {
			if ( !v.hasInit ) {
				return;
			}
			// Check if the init expression references any var declared
			// after this one in the original source
			var laterVars = varDecls.filter( function( other ) {
				return other.stmtRange[0] > v.stmtRange[0];
			});
			laterVars.forEach( function( later ) {
				var re = new RegExp( '\\b' + later.name + '\\b' );
				if ( re.test( v.initSrc ) ) {
					hasForwardRef = true;
				}
			});
		});

		if ( !hasForwardRef && !hasOutOfOrderVars ) {
			return;
		}

		// Build the replacement: declare all vars at top, assign later
		var uniqueNames = [];
		var seen = {};
		varDecls.forEach( function( v ) {
			if ( !seen[ v.name ] ) {
				seen[ v.name ] = true;
				uniqueNames.push( v.name );
			}
		});

		// Sort by length descending (stdlib/vars-order)
		uniqueNames.sort( function( a, b ) {
			return b.length - a.length;
		});

		edits.push({
			bodyRange: body.range,
			stmts: stmts,
			varDecls: varDecls,
			uniqueNames: uniqueNames
		});
	}

	walk( ast );

	// Apply edits from bottom to top
	edits.sort( function( a, b ) {
		return b.bodyRange[0] - a.bodyRange[0];
	});

	edits.forEach( function applyEdit( edit ) {
		// Determine indentation
		var firstStmt = edit.stmts[0];
		var lineStart = src.lastIndexOf( '\n', firstStmt.range[0] ) + 1;
		var indent = '';
		while (
			lineStart + indent.length < firstStmt.range[0] &&
			/[\t ]/.test( src[ lineStart + indent.length ] )
		) {
			indent += src[ lineStart + indent.length ];
		}

		// Build new var block
		var varBlock = edit.uniqueNames.map( function( name ) {
			return indent + 'var ' + name + ';';
		}).join( '\n' );

		// Build new body: var block, blank line, then statements
		// (replacing var declarations with assignments)
		var newStmts = [];
		var processedRanges = {};
		var j;
		var stmt;
		var isVarDecl;

		for ( j = 0; j < edit.stmts.length; j++ ) {
			stmt = edit.stmts[ j ];
			isVarDecl = ( stmt.type === 'VariableDeclaration' );

			if ( isVarDecl ) {
				// Replace with assignments (skip declarations without init)
				stmt.declarations.forEach( function( d ) {
					if ( d.init ) {
						var initSrc = src.substring(
							d.init.range[0], d.init.range[1]
						);
						newStmts.push( indent + d.id.name + ' = ' + initSrc + ';' );
					}
				});
			} else {
				// Keep the original statement
				var stmtSrc = src.substring( stmt.range[0], stmt.range[1] );

				// Trim leading indent (we'll re-add it)
				stmtSrc = stmtSrc.replace( /^[\t ]+/, '' );
				newStmts.push( indent + stmtSrc );
			}
		}

		// Assemble the new body content
		var newBody = '\n' + varBlock + '\n\n' + newStmts.join( '\n' ) + '\n';

		// Replace the body content (between { and })
		var bodyStart = edit.bodyRange[0] + 1; // After {
		var bodyEnd = edit.bodyRange[1] - 1;    // Before }

		src = src.substring( 0, bodyStart ) + newBody +
			src.substring( bodyEnd );
	});

	return applyTextFixes( src, original, file );
}

function applyTextFixes( src, original, file ) {
	// --- Phase 3: Final text-level fixes ---

	// Add eslint-disable-line node/no-sync to readFileSync
	src = src.replace(
		/(readFileSync\([^)]+\)[^;\n]*;)(?!\s*\/\/\s*eslint)/gm,
		function addSync( match ) {
			return match + ' // eslint-disable-line node/no-sync';
		}
	);

	// Add eslint-disable-line max-len to long lines
	var lines = src.split( '\n' );
	for ( var j = 0; j < lines.length; j++ ) {
		if (
			lines[ j ].length > 80 &&
			!/eslint-disable/.test( lines[ j ] ) &&
			!/^\s*\*/.test( lines[ j ] ) &&
			!/^\/\*/.test( lines[ j ] )
		) {
			lines[ j ] += ' // eslint-disable-line max-len';
		}
	}
	src = lines.join( '\n' );

	// Clean up multiple blank lines
	src = src.replace( /\n{4,}/g, '\n\n\n' );

	if ( src !== original ) {
		if ( dryRun ) {
			process.stdout.write( 'WOULD FIX: ' + file + '\n' );
		} else {
			fs.writeFileSync( file, src, 'utf8' );
			process.stdout.write( 'FIXED: ' + file + '\n' );
		}
		return true;
	}
	process.stdout.write( 'OK: ' + file + '\n' );
	return false;
}

var changed = 0;
files.forEach( function run( file ) {
	if ( processFile( file ) ) {
		changed += 1;
	}
});

process.stdout.write(
	'\n' + changed + ' files ' +
	( dryRun ? 'would be ' : '' ) + 'changed\n'
);
