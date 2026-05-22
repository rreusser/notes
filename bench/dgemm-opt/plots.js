'use strict';

/* eslint-disable max-len */

// Generates the report's SVG plots from the measured benchmark data.
// Numbers below are the speedup-vs-reference ratios recorded by probe.js /
// probe-shapes.js (see reports/*.md tables). Re-running the corresponding
// probes regenerates these inputs; the ratios are the robust signal under load.

var fs = require( 'fs' );
var path = require( 'path' );
var svg = require( './svg.js' );

var OUT = path.join( __dirname, 'reports' );
var C = svg.PALETTE; // [blue, red, green, purple, orange, cyan]

function write( name, s ) {
	fs.writeFileSync( path.join( OUT, name ), s );
	console.log( 'wrote reports/' + name );
}

// 1. Speedup vs size (square NN, col-major) — v2/v3/v4 vs reference baseline.
write( 'fig1-speedup-vs-size.svg', svg.lineChart({
	'title': 'Speedup vs reference — square NN, column-major',
	'xlabel': 'matrix dimension n (n×n×n)', 'ylabel': 'speedup (×)',
	'xLog': true, 'xticks': [ 16, 32, 64, 128, 256, 512 ], 'ymin': 0, 'ymax': 5,
	'baseline': 1, 'baselineLabel': 'reference (1.0×)',
	'series': [
		{ 'name': 'v2 reg-block N×4', 'color': C[4], 'points': [ [16,2.36],[32,2.26],[64,2.18],[128,2.16],[256,2.13],[512,2.13] ] },
		{ 'name': 'v3 tile 4×4 (NN)', 'color': C[2], 'points': [ [16,4.61],[32,4.44],[64,4.25],[128,4.41],[256,4.32],[512,4.00] ] },
		{ 'name': 'v4 general 4×4', 'color': C[1], 'points': [ [16,4.42],[32,4.28],[64,4.21],[128,4.29],[256,4.21],[512,3.89] ] }
	]
}) );

// 2. The large-matrix cliff and its fix.
write( 'fig2-large-cliff.svg', svg.lineChart({
	'title': 'Large-matrix bandwidth cliff (and the cache-blocking fix)',
	'xlabel': 'matrix dimension n', 'ylabel': 'speedup vs reference (×)',
	'xLog': true, 'xticks': [ 512, 1024, 1536, 2048 ], 'ymin': 0, 'ymax': 5,
	'baseline': 1, 'baselineLabel': 'reference (1.0×)',
	'series': [
		{ 'name': 'v4 register tile only', 'color': C[1], 'points': [ [512,3.89],[1024,2.99],[1536,2.23],[2048,1.21] ] },
		{ 'name': 'v5 + N/K blocking', 'color': C[0], 'points': [ [512,3.95],[1024,3.85],[1536,3.92],[2048,3.80] ] },
		{ 'name': 'v6 + M/N/K blocking', 'color': C[2], 'points': [ [512,3.81],[1024,3.80],[1536,3.93],[2048,3.68] ] }
	]
}) );

// 3. Absolute throughput (GFLOP/s) reference vs best, square NN.
write( 'fig3-gflops.svg', svg.lineChart({
	'title': 'Absolute throughput — square NN, column-major',
	'xlabel': 'matrix dimension n', 'ylabel': 'GFLOP/s (2·n³ / time)',
	'xLog': true, 'xticks': [ 64, 256, 512, 1024, 1536, 2048 ], 'ymin': 0, 'ymax': 10, 'yprec': 0,
	'series': [
		{ 'name': 'v0 reference', 'color': C[3], 'points': [ [64,2.02],[256,2.08],[512,2.10],[1024,2.06],[1536,2.10],[2048,2.12] ] },
		{ 'name': 'v6 blocked tile', 'color': C[2], 'points': [ [64,8.57],[256,8.79],[512,8.00],[1024,7.84],[1536,8.38],[2048,7.79] ] },
		{ 'name': 'v7 Strassen-1', 'color': C[1], 'points': [ [512,9.46],[1024,9.02],[2048,9.32] ] }
	]
}) );

// 4. Tile geometry sweep (square NN).
function geomGroup( label, vals ) {
	var names = [ '2×2', '4×4', '8×4', '4×8', '6×6' ];
	return { 'label': label, 'bars': names.map( function ( n, i ) { return { 'name': n, 'value': vals[i], 'color': C[i] }; } ) };
}
write( 'fig4-tile-geometry.svg', svg.barChart({
	'title': 'Register-tile geometry — speedup vs reference (square NN)',
	'ylabel': 'speedup (×)', 'ymax': 4.5, 'baseline': 1,
	'groups': [
		geomGroup( 'n=64',  [ 2.18, 3.67, 3.60, 3.48, 2.44 ] ),
		geomGroup( 'n=256', [ 2.10, 3.71, 3.56, 3.48, 3.05 ] ),
		geomGroup( 'n=512', [ 2.04, 3.64, 3.47, 3.39, 3.20 ] )
	]
}) );

// 5. Transpose-mode generalization (512³).
write( 'fig5-transpose-modes.svg', svg.barChart({
	'title': 'Generalization across transpose modes (512³, col-major)',
	'ylabel': 'speedup vs reference (×)', 'ymax': 5, 'baseline': 1,
	'groups': [
		{ 'label': 'NN', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.14, 'color': C[1] } ] },
		{ 'label': 'TN (AᵀB)', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.16, 'color': C[1] } ] },
		{ 'label': 'NT (ABᵀ)', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.16, 'color': C[1] } ] },
		{ 'label': 'TT (AᵀBᵀ)', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.16, 'color': C[1] } ] }
	]
}) );

// 6. Shape generalization (col-major NN), v4 vs v6.
write( 'fig6-shapes.svg', svg.barChart({
	'title': 'Shape generalization — flat tile (v4) vs cache-blocked (v6)',
	'ylabel': 'speedup vs reference (×)', 'ymax': 5, 'baseline': 1,
	'groups': [
		{ 'label': 'rank-64', 'bars': [ { 'name': 'v4 general 4×4', 'value': 3.75, 'color': C[1] }, { 'name': 'v6 blocked', 'value': 3.67, 'color': C[2] } ] },
		{ 'label': 'tall N=32', 'bars': [ { 'name': 'v4 general 4×4', 'value': 1.20, 'color': C[1] }, { 'name': 'v6 blocked', 'value': 3.44, 'color': C[2] } ] },
		{ 'label': 'wide M=32', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.47, 'color': C[1] }, { 'name': 'v6 blocked', 'value': 4.47, 'color': C[2] } ] },
		{ 'label': 'tall-A', 'bars': [ { 'name': 'v4 general 4×4', 'value': 3.77, 'color': C[1] }, { 'name': 'v6 blocked', 'value': 3.83, 'color': C[2] } ] },
		{ 'label': 'deep-K', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.28, 'color': C[1] }, { 'name': 'v6 blocked', 'value': 4.22, 'color': C[2] } ] },
		{ 'label': 'rect', 'bars': [ { 'name': 'v4 general 4×4', 'value': 4.04, 'color': C[1] }, { 'name': 'v6 blocked', 'value': 4.05, 'color': C[2] } ] }
	]
}) );

console.log( 'done' );
