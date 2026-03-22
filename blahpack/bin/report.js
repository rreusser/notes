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
			isStub = /TODO: implement|not yet implemented/.test( src );
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

		return {
			name: name,
			package: pkg,
			description: description,
			hasImpl: hasImpl && !isStub,
			isStub: isStub,
			hasTests: hasTests,
			testCount: testCount
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
var allModules = blasModules.concat( lapackModules );

var blasSources = scanSources( 'blas' );
var lapackSources = scanSources( 'lapack' );

var implemented = allModules.filter( function( m ) { return m.hasImpl; } );
var stubs = allModules.filter( function( m ) { return m.isStub; } );
var totalTests = allModules.reduce( function( s, m ) { return s + m.testCount; }, 0 );

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
	return '<tr>' +
		'<td class="name">' + m.name + '</td>' +
		'<td>' + status + '</td>' +
		'<td class="tests">' + tests + '</td>' +
		'<td class="desc">' + desc + '</td>' +
		'</tr>';
}

function sectionHTML( title, modules ) {
	var complete = modules.filter( function( m ) { return m.hasImpl; } ).length;
	var rows = modules.map( moduleRow ).join( '\n' );
	return '<section>\n' +
		'<h2>' + title + ' <span class="count">' + complete + '/' + modules.length + '</span></h2>\n' +
		'<table>\n<thead><tr><th>Routine</th><th>Status</th><th>Tests</th><th>Description</th></tr></thead>\n' +
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
'</style>\n</head>\n<body>\n' +
'<h1>Blahpack Translation Progress</h1>\n' +
'<p class="subtitle">Fortran BLAS/LAPACK &rarr; JavaScript &mdash; generated ' + now + '</p>\n' +
'<div class="stats">\n' +
'<div class="stat green"><div class="val">' + implemented.length + '</div><div class="lbl">Implemented</div></div>\n' +
'<div class="stat"><div class="val">' + stubs.length + '</div><div class="lbl">Stubs</div></div>\n' +
'<div class="stat"><div class="val">' + totalTests + '</div><div class="lbl">Tests</div></div>\n' +
'<div class="stat"><div class="val">' + blasSources.length + '</div><div class="lbl">BLAS Source</div></div>\n' +
'<div class="stat"><div class="val">' + lapackSources.length + '</div><div class="lbl">LAPACK Source</div></div>\n' +
'</div>\n' +
sectionHTML( 'BLAS', blasModules ) +
sectionHTML( 'LAPACK', lapackModules ) +
'</body>\n</html>\n';

process.stdout.write( html );
