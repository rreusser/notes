'use strict';

/* eslint-disable max-len */

// Layout-combination probe. dgemm performance depends on the memory layout of
// A, B, and C independently (row- vs column-major). The upstream stdlib kernel
// has a cache-optimal "naive" fast path specifically for A row-major + B
// column-major; everything else falls to loop tiling. This probe measures every
// (layoutA × layoutB) combination for the NN case so we can see where each
// implementation wins.

var harness = require( './harness.js' );

var VARIANTS = ( process.env.VARIANTS || 'upstream-stdlib,v0-reference,v6-blocked3lvl' ).split( ',' );
var N = Number( process.env.N || 256 );
var TRIALS = Number( process.env.TRIALS || 13 );

function randF64( n ) {
	var a = new Float64Array( n );
	var i;
	for ( i = 0; i < n; i++ ) { a[i] = ( Math.random()*20 ) - 10; }
	return a;
}

// strides for a (rows x cols) matrix in the given layout:
function strides( rows, cols, layout ) {
	if ( layout === 'row' ) { return { 's1': cols, 's2': 1, 'n': rows*cols }; }
	return { 's1': 1, 's2': rows, 'n': rows*cols }; // col-major
}

function makeFn( fn, la, lb, lc ) {
	var sa = strides( N, N, la );
	var sb = strides( N, N, lb );
	var sc = strides( N, N, lc );
	var A = randF64( sa.n );
	var B = randF64( sb.n );
	var C = randF64( sc.n );
	return function () {
		fn( 'no-transpose', 'no-transpose', N, N, N, 1.0, A, sa.s1, sa.s2, 0, B, sb.s1, sb.s2, 0, 1.0, C, sc.s1, sc.s2, 0 );
	};
}

var fns = VARIANTS.map( function ( v ) { return { 'name': v, 'fn': require( './variants/'+v+'.js' ) }; } );

var combos = [
	[ 'row', 'col' ], // upstream naive fast path
	[ 'row', 'row' ], // C-order / NumPy default
	[ 'col', 'col' ], // Fortran / classic BLAS
	[ 'col', 'row' ]
];

console.log( 'NN, n=' + N + ', GFLOP/s (and speedup vs ' + VARIANTS[0] + ')' );
console.log( [ 'A×B layout' ].concat( VARIANTS ).join( '\t' ) );
combos.forEach( function ( c ) {
	// keep C in column-major for all (typical output buffer):
	var bound = fns.map( function ( f ) { return { 'name': f.name, 'fn': makeFn( f.fn, c[0], c[1], 'col' ) }; } );
	var res = harness.benchmark({ 'variants': bound, 'trials': TRIALS, 'targetMs': 50, 'warmup': 3 });
	var gf = function ( name ) { return ( 2*N*N*N ) / res[name].minNs; };
	var cells = [ 'A=' + c[0] + ' B=' + c[1] ];
	VARIANTS.forEach( function ( v ) { cells.push( gf(v).toFixed(2) + ( v===VARIANTS[0] ? '' : ' ('+( res[VARIANTS[0]].minNs/res[v].minNs ).toFixed(2)+'x)' ) ); } );
	console.log( cells.join( '\t' ) );
} );
