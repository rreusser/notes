'use strict';

// Checks that base.js function signatures follow the expected expansion rules
// from Fortran to stdlib-js ndarray conventions:
//
//   Fortran array + LDA  →  JS: A, strideA1, strideA2, offsetA  (2D matrix)
//   Fortran array + INC  →  JS: x, strideX, offsetX             (1D vector)
//   Fortran CHARACTER*1  →  JS: lowercase name (trans, uplo)
//   Fortran scalar       →  JS: lowercase name (alpha, beta)
//   Fortran INTEGER dim  →  JS: uppercase name (M, N, K)
//   Fortran INFO (out)   →  skipped (returned as function value)
//   Fortran WORK/LWORK   →  skipped (internal workspace)
//
// The rule reads the routine's entry from data/routines.json to know what
// the Fortran signature is, then compares the structural pattern (not exact
// names, since BLAS uses DA/DX while JS uses alpha/x).

var path = require( 'path' );
var fs = require( 'fs' );

var DB_PATH = path.join( __dirname, '..', '..', '..', 'data', 'routines.json' );
var db;

function loadDB() {
	if ( db ) return db;
	if ( !fs.existsSync( DB_PATH ) ) return null;
	db = JSON.parse( fs.readFileSync( DB_PATH, 'utf8' ) );
	return db;
}

// Determine the "shape" of a Fortran argument: how many JS params it expands to
// Returns: 'skip' | 'scalar' | '1d' | '2d'
function argShape( arg, allArgs ) {
	var name = arg.name.toUpperCase();
	var type = arg.type;

	// Skip: INFO output, WORK arrays, LWORK/RWORK/IWORK sizes
	if ( /^INFO$/i.test( name ) ) return 'skip';
	if ( /^L?WORK$|^RWORK$|^IWORK$|^LRWORK$|^LIWORK$/i.test( name ) ) return 'skip';

	// Skip: leading dimensions (replaced by strides)
	if ( /^LD/i.test( name ) ) return 'skip';

	// Skip: increment parameters (replaced by strides)
	if ( /^INC/i.test( name ) ) return 'skip';

	var isArray = /array/i.test( type );
	if ( !isArray ) return 'scalar'; // CHARACTER, INTEGER, DOUBLE PRECISION scalar

	// Determine if 2D matrix: must have a corresponding LDx parameter
	// (the most reliable indicator — don't rely on dimension() patterns
	// since dimension(N) 1D arrays can look like dimension(expr,expr))
	var hasLD = allArgs.some( function( a ) {
		return new RegExp( '^LD' + name + '$', 'i' ).test( a.name );
	});

	return hasLD ? '2d' : '1d';
}

// Convert Fortran args to expected JS param count pattern
function expectedParamShapes( fArgs ) {
	var shapes = [];
	fArgs.forEach( function forEach( arg ) {
		var shape = argShape( arg, fArgs );
		if ( shape === 'skip' ) return;
		if ( shape === 'scalar' ) {
			shapes.push( { fortranName: arg.name, shape: 'scalar', count: 1 } );
		} else if ( shape === '1d' ) {
			shapes.push( { fortranName: arg.name, shape: '1d', count: 3 } ); // array, stride, offset
		} else if ( shape === '2d' ) {
			shapes.push( { fortranName: arg.name, shape: '2d', count: 4 } ); // array, stride1, stride2, offset
		}
	});
	return shapes;
}

// Extract function parameter names from a FunctionDeclaration AST node
function getFunctionParams( node ) {
	if ( !node.params ) return [];
	return node.params.map( function map( p ) {
		return p.name || ( p.type === 'AssignmentPattern' && p.left ? p.left.name : '?' );
	});
}

var rule = {
	'meta': {
		'docs': {
			'description': 'verify base.js function signatures match Fortran-to-ndarray expansion rules'
		},
		'schema': [],
		'type': 'problem'
	},
	'create': function main( context ) {
		var filename = context.getFilename();
		var basename = path.basename( filename );

		// Only check base.js files
		if ( basename !== 'base.js' ) return {};

		// Determine routine name from path
		var parts = filename.split( path.sep );
		var baseIdx = parts.indexOf( 'base' );
		if ( baseIdx === -1 || baseIdx + 1 >= parts.length ) return {};
		var routineName = parts[ baseIdx + 1 ];

		// Load DB and find the variant
		var data = loadDB();
		if ( !data || !data.routines ) return {};

		var fVariant = null;
		var algKeys = Object.keys( data.routines );
		for ( var i = 0; i < algKeys.length; i++ ) {
			var alg = data.routines[ algKeys[i] ];
			var variants = alg.variants || [];
			for ( var j = 0; j < variants.length; j++ ) {
				if ( variants[j].name.toLowerCase() === routineName ) {
					fVariant = variants[j];
					break;
				}
			}
			if ( fVariant ) break;
		}

		if ( !fVariant ) return {}; // Not in DB

		var expectedShapes = expectedParamShapes( fVariant.arguments );
		var expectedCount = expectedShapes.reduce( function( sum, s ) {
			return sum + s.count;
		}, 0 );

		var lastTopLevelFunc = null;

		return {
			'FunctionDeclaration': function onFunc( node ) {
				// Track top-level function declarations (the last one is the export)
				if ( node.parent.type !== 'Program' ) return;
				lastTopLevelFunc = node;
			},
			'Program:exit': function onExit() {
				var node = lastTopLevelFunc;
				if ( !node ) return;

				var jsParams = getFunctionParams( node );
				var actualCount = jsParams.length;

				// Check parameter count
				if ( actualCount !== expectedCount ) {
					context.report({
						'node': node,
						'message': 'Signature has ' + actualCount + ' params, expected ' + expectedCount + ' based on Fortran signature (' + fVariant.name + '). ' +
							'Expected pattern: ' + expectedShapes.map( function( s ) {
								if ( s.shape === 'scalar' ) return s.fortranName.toLowerCase();
								if ( s.shape === '1d' ) return s.fortranName + ',stride,offset';
								return s.fortranName + ',stride1,stride2,offset';
							}).join( ', ' )
					});
					return;
				}

				// Check structural pattern: verify stride/offset naming conventions
				var pos = 0;
				expectedShapes.forEach( function forEach( shape ) {
					if ( pos >= jsParams.length ) return;

					if ( shape.shape === '1d' ) {
						var arrName = jsParams[ pos ];
						var strideName = jsParams[ pos + 1 ];
						var offsetName = jsParams[ pos + 2 ];

						// Check stride naming: should be stride<Name>
						if ( !/^stride/i.test( strideName ) ) {
							context.report({
								'node': node.params[ pos + 1 ],
								'message': 'Expected stride parameter for 1D array "' + arrName + '", got "' + strideName + '". Should match pattern: stride<Name>'
							});
						}
						// Check offset naming: should be offset<Name>
						if ( !/^offset/i.test( offsetName ) ) {
							context.report({
								'node': node.params[ pos + 2 ],
								'message': 'Expected offset parameter for 1D array "' + arrName + '", got "' + offsetName + '". Should match pattern: offset<Name>'
							});
						}
					} else if ( shape.shape === '2d' ) {
						var matName = jsParams[ pos ];
						var s1 = jsParams[ pos + 1 ];
						var s2 = jsParams[ pos + 2 ];
						var off = jsParams[ pos + 3 ];

						if ( !/^stride/i.test( s1 ) || !/1$/.test( s1 ) ) {
							context.report({
								'node': node.params[ pos + 1 ],
								'message': 'Expected stride1 parameter for 2D array "' + matName + '", got "' + s1 + '". Should match: stride<Name>1'
							});
						}
						if ( !/^stride/i.test( s2 ) || !/2$/.test( s2 ) ) {
							context.report({
								'node': node.params[ pos + 2 ],
								'message': 'Expected stride2 parameter for 2D array "' + matName + '", got "' + s2 + '". Should match: stride<Name>2'
							});
						}
						if ( !/^offset/i.test( off ) ) {
							context.report({
								'node': node.params[ pos + 3 ],
								'message': 'Expected offset parameter for 2D array "' + matName + '", got "' + off + '". Should match: offset<Name>'
							});
						}
					}
					pos += shape.count;
				});
			}
		};
	}
};

module.exports = rule;
