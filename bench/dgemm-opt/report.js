'use strict';

/* eslint-disable max-len */

// Comprehensive benchmark report generator for dgemm variants.
// Runs a battery of shape/transpose/layout sweeps with the min-of-trials
// harness and writes a markdown report to reports/<name>.md.
//
// Usage: node report.js [outfile]

var fs = require( 'fs' );
var path = require( 'path' );
var os = require( 'os' );
var cp = require( 'child_process' );
var run = require( './run.js' );

var BASE = 'v0-reference';

function fmt( x, n ) { return x.toFixed( n === undefined ? 3 : n ); }

function loadAvg() {
	return os.loadavg().map( function ( x ) { return x.toFixed( 2 ); } ).join( ', ' );
}

// Render a sweep result as a markdown table comparing GFLOPS + speedup of each
// non-base variant against BASE.
function table( rows, variants ) {
	var others = variants.filter( function ( v ) { return v !== BASE; } );
	var out = [];
	var head = [ 'shape', 'M', 'N', 'K', BASE+' (GF/s)' ];
	others.forEach( function ( v ) { head.push( v+' (GF/s)' ); } );
	others.forEach( function ( v ) { head.push( v+' speedup' ); } );
	out.push( '| ' + head.join( ' | ' ) + ' |' );
	out.push( '|' + head.map( function () { return '---'; } ).join( '|' ) + '|' );
	rows.forEach( function ( r ) {
		var cells = [ r.label, r.M, r.N, r.K, fmt( r.variants[BASE].gflops ) ];
		others.forEach( function ( v ) { cells.push( fmt( r.variants[v].gflops ) ); } );
		others.forEach( function ( v ) { cells.push( fmt( r.variants[BASE].minNs / r.variants[v].minNs )+'x' ); } );
		out.push( '| ' + cells.join( ' | ' ) + ' |' );
	} );
	return out.join( '\n' );
}

function meanSpeedup( rows, v ) {
	var s = 0;
	rows.forEach( function ( r ) { s += r.variants[BASE].minNs / r.variants[v].minNs; } );
	return s / rows.length;
}

function doSweep( variants, shapes, transa, transb, layout, opts ) {
	opts = opts || {};
	return run.sweep({
		'variants': variants,
		'shapes': shapes,
		'transa': transa,
		'transb': transb,
		'layout': layout,
		'alpha': 1.0,
		'beta': 1.0,
		'trials': opts.trials || 13,
		'targetMs': opts.targetMs || 50
	});
}

function main() {
	var outfile = process.argv[2] || path.join( __dirname, 'reports', 'dgemm-optimization.md' );
	var variants = process.env.VARIANTS ? process.env.VARIANTS.split( ',' ) : [ BASE, 'v4-general4x4' ];
	var md = [];
	var sq, modes, shapesS;

	md.push( '# dgemm optimization report' );
	md.push( '' );
	md.push( '_Generated ' + new Date().toISOString() + '_' );
	md.push( '' );
	md.push( '## Environment' );
	md.push( '' );
	md.push( '- Node: `' + process.version + '`' );
	md.push( '- CPU: `' + os.cpus()[0].model + '` (' + os.cpus().length + ' logical)' );
	md.push( '- Platform: `' + process.platform + ' ' + os.release() + '`' );
	md.push( '- Load average at start: `' + loadAvg() + '`' );
	md.push( '' );
	md.push( '> **Isolation note.** This machine is shared and was under heavy concurrent load during measurement. The harness uses **minimum-of-trials** timing (external contention can only add wall time, so the minimum over many interleaved trials best estimates the true cost) and **round-robin interleaving** of variants (so slow drift hits all variants equally). Absolute GF/s should be read as a floor; the **speedup ratios are the robust signal** because both variants run back-to-back under identical conditions.' );
	md.push( '' );
	md.push( '## Variants' );
	md.push( '' );
	variants.forEach( function ( v ) {
		var first = fs.readFileSync( path.join( __dirname, 'variants', v+'.js' ), 'utf8' ).split( '\n' );
		var desc = first.filter( function ( l ) { return /^\/\/ /.test( l ); } ).slice( 0, 1 ).join( ' ' ).replace( /^\/\/ /, '' );
		md.push( '- **`' + v + '`** — ' + ( desc || '(see source)' ) );
	} );
	md.push( '' );
	md.push( 'GFLOP/s = 2·M·N·K / time. FLOP count is identical across variants, so GF/s is directly comparable.' );
	md.push( '' );

	// 1. Square NN col-major sweep:
	process.stderr.write( '[1/6] square NN col-major\n' );
	var squares = [ 16, 32, 48, 64, 96, 128, 192, 256, 384, 512, 768, 1024 ].map( function ( n ) { return { 'M': n, 'N': n, 'K': n, 'label': n+'^3' }; } );
	sq = doSweep( variants, squares, 'no-transpose', 'no-transpose', 'col' );
	md.push( '## 1. Square matrices, NN, column-major' );
	md.push( '' );
	md.push( table( sq, variants ) );
	md.push( '' );
	variants.filter( function ( v ) { return v !== BASE; } ).forEach( function ( v ) {
		md.push( '- `' + v + '` mean speedup: **' + fmt( meanSpeedup( sq, v ), 2 ) + 'x**' );
	} );
	md.push( '' );

	// 2. Transpose-mode generalization (fixed size):
	process.stderr.write( '[2/6] transpose modes\n' );
	md.push( '## 2. Transpose-mode generalization (512^3, column-major)' );
	md.push( '' );
	var tmHead = [ 'mode', BASE+' (GF/s)' ].concat( variants.filter(function(v){return v!==BASE;}).map(function(v){return v+' speedup';}) );
	var tm = [ '| ' + tmHead.join( ' | ' ) + ' |', '|' + tmHead.map(function(){return '---';}).join('|') + '|' ];
	modes = [ [ 'NN', 'no-transpose', 'no-transpose' ], [ 'TN (AᵀB)', 'transpose', 'no-transpose' ], [ 'NT (ABᵀ)', 'no-transpose', 'transpose' ], [ 'TT (AᵀBᵀ)', 'transpose', 'transpose' ] ];
	modes.forEach( function ( m ) {
		var r = doSweep( variants, [ { 'M': 512, 'N': 512, 'K': 512, 'label': m[0] } ], m[1], m[2], 'col' )[0];
		var cells = [ m[0], fmt( r.variants[BASE].gflops ) ];
		variants.filter(function(v){return v!==BASE;}).forEach( function ( v ) { cells.push( fmt( r.variants[BASE].minNs/r.variants[v].minNs )+'x' ); } );
		tm.push( '| ' + cells.join( ' | ' ) + ' |' );
	} );
	md.push( tm.join( '\n' ) );
	md.push( '' );

	// 3. Layout: row-major NN
	process.stderr.write( '[3/6] row-major\n' );
	md.push( '## 3. Layout generalization: row-major, NN' );
	md.push( '' );
	var rm = doSweep( variants, [ 128, 256, 512 ].map(function(n){return {M:n,N:n,K:n,label:n+'^3'};}), 'no-transpose', 'no-transpose', 'row' );
	md.push( table( rm, variants ) );
	md.push( '' );

	// 4. Non-square shapes (col-major NN):
	process.stderr.write( '[4/6] non-square shapes\n' );
	md.push( '## 4. Shape generalization (column-major, NN)' );
	md.push( '' );
	shapesS = [
		{ 'M': 1024, 'N': 1024, 'K': 16, 'label': 'rank-16 update' },
		{ 'M': 1024, 'N': 1024, 'K': 64, 'label': 'rank-64 update' },
		{ 'M': 1024, 'N': 16, 'K': 1024, 'label': 'tall*skinny (N=16)' },
		{ 'M': 16, 'N': 1024, 'K': 1024, 'label': 'short*wide (M=16)' },
		{ 'M': 2048, 'N': 64, 'K': 64, 'label': 'tall A panel' },
		{ 'M': 64, 'N': 64, 'K': 2048, 'label': 'deep inner (K=2048)' },
		{ 'M': 512, 'N': 256, 'K': 128, 'label': 'rectangular' }
	];
	var ss = doSweep( variants, shapesS, 'no-transpose', 'no-transpose', 'col' );
	md.push( table( ss, variants ) );
	md.push( '' );

	// 5. Small matrices (overhead regime):
	process.stderr.write( '[5/6] small matrices\n' );
	md.push( '## 5. Small matrices (fixed-overhead regime, col-major NN)' );
	md.push( '' );
	var sm = doSweep( variants, [ 2, 3, 4, 5, 6, 8, 12 ].map(function(n){return {M:n,N:n,K:n,label:n+'^3'};}), 'no-transpose', 'no-transpose', 'col', { 'trials': 15 } );
	md.push( table( sm, variants ) );
	md.push( '' );

	// 6. Summary
	process.stderr.write( '[6/6] writing report\n' );
	md.push( '## Summary' );
	md.push( '' );
	variants.filter(function(v){return v!==BASE;}).forEach( function ( v ) {
		md.push( '- **`' + v + '`**: square mean **' + fmt( meanSpeedup( sq, v ), 2 ) + 'x**, non-square mean **' + fmt( meanSpeedup( ss, v ), 2 ) + 'x**, small-matrix mean **' + fmt( meanSpeedup( sm, v ), 2 ) + 'x**.' );
	} );
	md.push( '' );

	fs.writeFileSync( outfile, md.join( '\n' ) );
	process.stderr.write( 'wrote ' + outfile + '\n' );
}

main();
