'use strict';

// Shape-generalization probe: compares variants across non-square shapes.
var run = require( './run.js' );

var variants = process.env.VARIANTS ? process.env.VARIANTS.split( ',' ) : [ 'v0-reference', 'v5-blocked4x4', 'v6-blocked3lvl' ];
var shapes = [
	{ 'M': 1024, 'N': 1024, 'K': 16, 'label': 'rank-16' },
	{ 'M': 1024, 'N': 1024, 'K': 64, 'label': 'rank-64' },
	{ 'M': 2048, 'N': 32, 'K': 2048, 'label': 'tall (N=32)' },
	{ 'M': 32, 'N': 2048, 'K': 2048, 'label': 'wide (M=32)' },
	{ 'M': 4096, 'N': 64, 'K': 64, 'label': 'tall-A panel' },
	{ 'M': 64, 'N': 64, 'K': 4096, 'label': 'deep-K' },
	{ 'M': 768, 'N': 384, 'K': 192, 'label': 'rectangular' }
];

var rows = run.sweep({
	'variants': variants, 'shapes': shapes,
	'transa': 'no-transpose', 'transb': 'no-transpose', 'layout': 'col',
	'alpha': 1.0, 'beta': 1.0, 'trials': Number( process.env.TRIALS || 11 ), 'targetMs': Number( process.env.TARGET || 60 )
});

var base = variants[0];
console.log( [ 'shape' ].concat( variants.map(function(v){return v+' GFs';}) ).concat( variants.slice(1).map(function(v){return v+' x';}) ).join( '\t' ) );
rows.forEach( function ( r ) {
	var cells = [ r.label ];
	variants.forEach( function ( v ) { cells.push( r.variants[v].gflops.toFixed(3) ); } );
	variants.slice(1).forEach( function ( v ) { cells.push( ( r.variants[base].minNs / r.variants[v].minNs ).toFixed(3) ); } );
	console.log( cells.join( '\t' ) );
} );
