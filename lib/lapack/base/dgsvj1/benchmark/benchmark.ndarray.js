/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var pkg = require( './../package.json' ).name;
var dgsvj1 = require( './../lib/ndarray.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};
var EPS = 2.220446049250313e-16;
var SFMIN = 2.2250738585072014e-308;
var TOL = 1.0e-10;


// FUNCTIONS //

/**
* Creates a benchmark function for an `N`x`N` matrix.
*
* @private
* @param {PositiveInteger} N - matrix dimension
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var work;
	var sva;
	var n1;
	var A;
	var d;
	var V;
	var i;

	A = uniform( N * N, -1.0, 1.0, options );
	d = new Float64Array( N );
	for ( i = 0; i < N; i++ ) {
		d[ i ] = 1.0;
	}
	sva = new Float64Array( N );
	for ( i = 0; i < N; i++ ) {
		sva[ i ] = 1.0;
	}
	V = new Float64Array( N * N );
	for ( i = 0; i < N; i++ ) {
		V[ ( i * N ) + i ] = 1.0;
	}
	work = new Float64Array( N );
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
			info = dgsvj1( 'no-v', N, N, n1, A, 1, N, 0, d, 1, 0, sva, 1, 0, 0, V, 1, N, 0, EPS, SFMIN, TOL, 1, work, 1, 0, N );
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
	max = 6; // 2^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 2, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:N=%d', pkg, len ), f );
	}
}

main();
