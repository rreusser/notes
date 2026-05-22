'use strict';

var run = require( './run.js' );

var variants = process.env.VARIANTS ? process.env.VARIANTS.split( ',' ) : [ 'v0-reference', 'v1-unitstride', 'v2-regblock-n4' ];
var sizes = process.env.SIZES ? process.env.SIZES.split( ',' ).map( Number ) : [ 16, 32, 64, 128, 256, 512 ];

var shapes = sizes.map( function ( n ) { return { 'M': n, 'N': n, 'K': n, 'label': n+'^3' }; } );

var rows = run.sweep({
	'variants': variants,
	'shapes': shapes,
	'transa': process.env.TA || 'no-transpose',
	'transb': process.env.TB || 'no-transpose',
	'layout': process.env.LAYOUT || 'col',
	'alpha': 1.0,
	'beta': 1.0,
	'trials': Number( process.env.TRIALS || 15 ),
	'targetMs': Number( process.env.TARGET || 50 )
});

// Print table:
var base = variants[ 0 ];
var hdr = [ 'shape' ].concat( variants.map( function ( v ) { return v + ' GFs'; } ) ).concat( variants.slice(1).map( function ( v ) { return v + ' x'; } ) );
console.log( hdr.join( '\t' ) );
rows.forEach( function ( r ) {
	var cells = [ r.label ];
	variants.forEach( function ( v ) { cells.push( r.variants[v].gflops.toFixed(3) ); } );
	variants.slice(1).forEach( function ( v ) {
		cells.push( ( r.variants[base].minNs / r.variants[v].minNs ).toFixed(3) );
	} );
	console.log( cells.join( '\t' ) );
} );
