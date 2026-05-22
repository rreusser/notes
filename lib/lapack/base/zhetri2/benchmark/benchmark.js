/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zhetri2 = require( './../lib/zhetri2.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var ldwork;
	var IPIV;
	var WORK;
	var nb;
	var A;
	var i;

	nb = 32;
	ldwork = N + nb + 1;

	// Build an N-by-N factored matrix with 1x1 pivots (IPIV[i] = i).
	A = new Complex128Array( N * N );
	for ( i = 0; i < N; i++ ) {
		A.set( [ 1.0 + i, 0.0 ], ( i * N ) + i );
	}
	IPIV = new Int32Array( N );
	for ( i = 0; i < N; i++ ) {
		IPIV[ i ] = i;
	}
	// Provision enough WORK to cover whichever dispatch path is taken.
	WORK = new Complex128Array( ldwork * ( nb + 3 ) );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var info;
		var i;

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			info = zhetri2( 'column-major', 'lower', N, A, N, IPIV, 1, 0, WORK, 1 );
			if ( isnan( info ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( info ) ) {
			b.fail( 'should not return NaN' );
		}
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

	min = 1; // 10^min
	max = 2; // 10^max (max N=100 to bound runtime)

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:len=%d', pkg, len ), f );
	}
}

main();
