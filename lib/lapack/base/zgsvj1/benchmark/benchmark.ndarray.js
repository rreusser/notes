/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zgsvj1 = require( './../lib/ndarray.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};
var EPS = 2.220446049250313e-16;
var SFMIN = 2.2250738585072014e-308;
var TOL = 1.0e-10;


// FUNCTIONS //

/**
* Creates a benchmark function for an `N`x`N` complex matrix.
*
* @private
* @param {PositiveInteger} N - matrix dimension
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var work;
	var view;
	var raw;
	var sva;
	var n1;
	var A;
	var d;
	var V;
	var i;

	raw = uniform( 2 * N * N, -1.0, 1.0, options );
	A = new Complex128Array( raw.buffer );
	d = new Complex128Array( N );
	for ( i = 0; i < N; i++ ) {
		reinterpret( d, 0 )[ 2 * i ] = 1.0;
	}
	sva = new Float64Array( N );
	for ( i = 0; i < N; i++ ) {
		sva[ i ] = 1.0;
	}
	V = new Complex128Array( N * N );
	view = reinterpret( V, 0 );
	for ( i = 0; i < N; i++ ) {
		view[ 2 * ( ( i * N ) + i ) ] = 1.0;
	}
	work = new Complex128Array( N );
	n1 = N >> 1;

	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var info;
		var k;
		b.tic();
		for ( k = 0; k < b.iterations; k++ ) {
			info = zgsvj1( 'no-v', N, N, n1, A, 1, N, 0, d, 1, 0, sva, 1, 0, 0, V, 1, N, 0, EPS, SFMIN, TOL, 1, work, 1, 0, N );
			if ( isnan( info ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		b.pass( 'benchmark finished' );
		b.end();
	}
}


// MAIN //

/**
* Main execution sequence.
*
* @private
*/
function main() {
	var len;
	var min;
	var max;
	var f;
	var i;

	min = 2; // 2^min
	max = 5; // 2^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 2, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:N=%d', pkg, len ), f );
	}
}

main();
