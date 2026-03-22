#!/usr/bin/env node

/**
* Generate a static HTML progress report.
*
* Usage:
*   node bin/report.js > progress.html
*
* Scans lib/ for implemented modules, reads package.json for descriptions,
* checks for test files, and cross-references against the full BLAS/LAPACK
* source inventory.
*/

'use strict';

var fs = require( 'fs' );
var path = require( 'path' );
var execSync = require( 'child_process' ).execSync;

var ROOT = path.join( __dirname, '..' );

// ---------------------------------------------------------------------------
// Scan implemented modules
// ---------------------------------------------------------------------------

function scanModules( pkg ) {
	var baseDir = path.join( ROOT, 'lib', pkg, 'base' );
	if ( !fs.existsSync( baseDir ) ) {
		return [];
	}
	var dirs = fs.readdirSync( baseDir ).filter( function( d ) {
		return fs.statSync( path.join( baseDir, d ) ).isDirectory();
	});
	return dirs.map( function( name ) {
		var dir = path.join( baseDir, name );
		var baseJs = path.join( dir, 'lib', 'base.js' );
		var pkgJson = path.join( dir, 'package.json' );
		var testJs = path.join( dir, 'test', 'test.js' );

		var hasImpl = false;
		var isStub = true;
		if ( fs.existsSync( baseJs ) ) {
			hasImpl = true;
			var src = fs.readFileSync( baseJs, 'utf8' );
			// Only count as stub if the MAIN function body is a stub,
			// not STOREV=R "not yet implemented" throws
			isStub = /^\t(?:\/\/ )?TODO: implement|^\tthrow new Error\( 'not yet implemented' \)/m.test( src );
		}

		var description = '';
		if ( fs.existsSync( pkgJson ) ) {
			try {
				description = JSON.parse( fs.readFileSync( pkgJson, 'utf8' ) ).description || '';
			} catch ( e ) {}
		}

		var hasTests = fs.existsSync( testJs );
		var testCount = 0;
		if ( hasTests ) {
			var testSrc = fs.readFileSync( testJs, 'utf8' );
			testCount = ( testSrc.match( /\btest\s*\(/g ) || [] ).length;
		}

		// Get coverage data if tests exist
		var lineCov = null;
		var branchCov = null;
		if ( hasTests && hasImpl && !isStub ) {
			try {
				var covOut = execSync(
					'node --test --experimental-test-coverage ' + testJs + ' 2>&1',
					{ timeout: 30000, encoding: 'utf8' }
				);
				// Parse coverage for base.js specifically
				// The module's own base.js appears right after the module
				// directory header (e.g. "ℹ    zlange       |"). Take the
				// FIRST base.js line that follows the module name header,
				// not the last (which may be a dependency's base.js).
				var covLines = covOut.split( '\n' );
				var foundDir = false;
				for ( var li = 0; li < covLines.length; li++ ) {
					var covLine = covLines[ li ];
					// Detect the module directory header
					if ( new RegExp( '\\b' + name + '\\b' ).test( covLine ) && /\|/.test( covLine ) ) {
						foundDir = true;
						continue;
					}
					if ( foundDir && /base\.js/.test( covLine ) && /\d+\.\d+/.test( covLine ) ) {
						var nums = covLine.match( /(\d+\.?\d*)/g );
						if ( nums && nums.length >= 3 ) {
							lineCov = parseFloat( nums[ 0 ] );
							branchCov = parseFloat( nums[ 1 ] );
						}
						break; // Take the first base.js after the module header
					}
				}
			} catch ( e ) {
				// Coverage run failed, skip
			}
		}

		return {
			name: name,
			package: pkg,
			description: description,
			hasImpl: hasImpl && !isStub,
			isStub: isStub,
			hasTests: hasTests,
			testCount: testCount,
			lineCov: lineCov,
			branchCov: branchCov
		};
	});
}

// ---------------------------------------------------------------------------
// Scan source inventory
// ---------------------------------------------------------------------------

function scanSources( pkg ) {
	var routines = {};
	var dirs;
	if ( pkg === 'blas' ) {
		dirs = [ path.join( ROOT, 'data', 'BLAS-3.12.0' ) ];
	} else {
		dirs = [ path.join( ROOT, 'data', 'lapack-3.12.0', 'SRC' ) ];
	}
	dirs.forEach( function( dir ) {
		if ( !fs.existsSync( dir ) ) return;
		fs.readdirSync( dir ).forEach( function( f ) {
			if ( /\.(f|f90)$/i.test( f ) ) {
				var name = f.replace( /\.(f|f90)$/i, '' ).toLowerCase();
				routines[ name ] = true;
			}
		});
	});
	return Object.keys( routines ).sort();
}

// ---------------------------------------------------------------------------
// Generate HTML
// ---------------------------------------------------------------------------

var blasModules = scanModules( 'blas' );
var lapackModules = scanModules( 'lapack' );

var blasSources = scanSources( 'blas' );
var lapackSources = scanSources( 'lapack' );

// Merge: include ALL reference routines, marking unimplemented ones
function mergeWithSources( modules, sources ) {
	var byName = {};
	modules.forEach( function( m ) {
		byName[ m.name ] = m;
	});
	return sources.map( function( name ) {
		if ( byName[ name ] ) {
			return byName[ name ];
		}
		return {
			name: name,
			package: modules.length > 0 ? modules[ 0 ].package : '',
			description: '',
			hasImpl: false,
			isStub: false,
			hasTests: false,
			testCount: 0,
			lineCov: null,
			branchCov: null
		};
	});
}

blasModules = mergeWithSources( blasModules, blasSources );
lapackModules = mergeWithSources( lapackModules, lapackSources );
var allModules = blasModules.concat( lapackModules );

var implemented = allModules.filter( function( m ) { return m.hasImpl; } );
var stubs = allModules.filter( function( m ) { return m.isStub; } );
var totalTests = allModules.reduce( function( s, m ) { return s + m.testCount; }, 0 );

function covBadge( val, threshold ) {
	if ( val === null ) return '<span class="cov">—</span>';
	var cls = val >= threshold ? 'cov-good' : val >= threshold - 10 ? 'cov-warn' : 'cov-bad';
	return '<span class="' + cls + '">' + val.toFixed( 0 ) + '%</span>';
}

function moduleRow( m ) {
	var status;
	if ( m.hasImpl ) {
		status = '<span class="badge done">done</span>';
	} else if ( m.isStub ) {
		status = '<span class="badge stub">stub</span>';
	} else {
		status = '<span class="badge none">—</span>';
	}
	var tests = m.testCount > 0 ? m.testCount : '—';
	var desc = m.description
		.replace( /TODO: Add description for \w+\./, '' )
		.replace( /&/g, '&amp;' )
		.replace( /</g, '&lt;' );
	var implAttr = ( m.hasImpl || m.isStub ) ? '' : ' class="unimpl"';
	return '<tr' + implAttr + '>' +
		'<td class="name">' + m.name + '</td>' +
		'<td>' + status + '</td>' +
		'<td class="tests">' + tests + '</td>' +
		'<td class="cov">' + covBadge( m.lineCov, 90 ) + '</td>' +
		'<td class="cov">' + covBadge( m.branchCov, 85 ) + '</td>' +
		'<td class="desc">' + desc + '</td>' +
		'</tr>';
}

function sectionHTML( title, modules ) {
	var complete = modules.filter( function( m ) { return m.hasImpl; } ).length;
	var rows = modules.map( moduleRow ).join( '\n' );
	return '<section>\n' +
		'<h2>' + title + ' <span class="count">' + complete + '/' + modules.length + '</span></h2>\n' +
		'<table>\n<thead><tr><th>Routine</th><th>Status</th><th>Tests</th><th>Lines</th><th>Branch</th><th>Description</th></tr></thead>\n' +
		'<tbody>\n' + rows + '\n</tbody>\n</table>\n</section>\n';
}

var now = new Date().toISOString().replace( /T.*/, '' );

var html = '<!DOCTYPE html>\n' +
'<html lang="en">\n<head>\n<meta charset="utf-8">\n' +
'<title>Blahpack Progress</title>\n' +
'<style>\n' +
'* { margin: 0; padding: 0; box-sizing: border-box; }\n' +
'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; background: #f8f9fa; color: #333; padding: 2rem; max-width: 960px; margin: 0 auto; }\n' +
'h1 { margin-bottom: 0.25rem; }\n' +
'.subtitle { color: #666; margin-bottom: 1.5rem; }\n' +
'.stats { display: flex; gap: 1.5rem; margin-bottom: 2rem; flex-wrap: wrap; }\n' +
'.stat { background: white; border-radius: 8px; padding: 1rem 1.5rem; box-shadow: 0 1px 3px rgba(0,0,0,0.08); }\n' +
'.stat .val { font-size: 1.75rem; font-weight: 700; }\n' +
'.stat .lbl { font-size: 0.8rem; color: #888; }\n' +
'.stat.green .val { color: #16a34a; }\n' +
'section { margin-bottom: 2rem; }\n' +
'h2 { margin-bottom: 0.5rem; }\n' +
'.count { font-weight: 400; color: #888; font-size: 0.9rem; }\n' +
'table { width: 100%; border-collapse: collapse; background: white; border-radius: 6px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.08); }\n' +
'th { text-align: left; padding: 0.5rem 0.75rem; background: #f1f3f5; font-size: 0.8rem; text-transform: uppercase; color: #666; }\n' +
'td { padding: 0.4rem 0.75rem; border-top: 1px solid #eee; font-size: 0.875rem; }\n' +
'.name { font-family: ui-monospace, monospace; font-weight: 600; }\n' +
'.tests { text-align: center; }\n' +
'.desc { color: #555; }\n' +
'.badge { display: inline-block; padding: 2px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 600; }\n' +
'.badge.done { background: #dcfce7; color: #166534; }\n' +
'.badge.stub { background: #fef9c3; color: #854d0e; }\n' +
'.badge.none { background: #f3f4f6; color: #9ca3af; }\n' +
'.cov { text-align: center; font-size: 0.8rem; }\n' +
'.cov-good { color: #16a34a; font-weight: 600; }\n' +
'.cov-warn { color: #ca8a04; font-weight: 600; }\n' +
'.cov-bad { color: #dc2626; font-weight: 600; }\n' +
'tr.unimpl { display: none; }\n' +
'body.show-all tr.unimpl { display: table-row; }\n' +
'.toggle { margin-bottom: 1.5rem; font-size: 0.9rem; cursor: pointer; user-select: none; }\n' +
'.toggle input { margin-right: 0.4rem; cursor: pointer; }\n' +
'</style>\n</head>\n<body>\n' +
'<h1>Blahpack Translation Progress</h1>\n' +
'<p class="subtitle">Fortran BLAS/LAPACK &rarr; JavaScript &mdash; generated ' + now + '</p>\n' +
'<div class="stats">\n' +
'<div class="stat green"><div class="val">' + implemented.length + '/' + ( blasSources.length + lapackSources.length ) + '</div><div class="lbl">Implemented</div></div>\n' +
'<div class="stat"><div class="val">' + totalTests + '</div><div class="lbl">Tests</div></div>\n' +
'<div class="stat"><div class="val">' + blasModules.filter( function( m ) { return m.hasImpl; } ).length + '/' + blasSources.length + '</div><div class="lbl">BLAS</div></div>\n' +
'<div class="stat"><div class="val">' + lapackModules.filter( function( m ) { return m.hasImpl; } ).length + '/' + lapackSources.length + '</div><div class="lbl">LAPACK</div></div>\n' +
'</div>\n' +
'<label class="toggle"><input type="checkbox" id="showAll">Show all reference routines (including unimplemented)</label>\n' +
sectionHTML( 'BLAS', blasModules ) +
sectionHTML( 'LAPACK', lapackModules ) +
'<script>document.getElementById("showAll").addEventListener("change",function(){document.body.classList.toggle("show-all",this.checked)});</script>\n' +
'</body>\n</html>\n';

process.stdout.write( html );
