/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zsyconvf_rook = require( './../lib/ndarray.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var IPIV = new Int32Array( N );
	var A = new Complex128Array( N * N );
	var E = new Complex128Array( N );
	var i;
	for ( i = 0; i < N; i++ ) {
		IPIV[ i ] = i;
	}
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	* @returns {void}
	*/
	function benchmark( b ) {
		var info;
		var j;
		b.tic();
		for ( j = 0; j < b.iterations; j++ ) {
			info = zsyconvf_rook( 'upper', 'convert', N, A, 1, N, 0, E, 1, 0, IPIV, 1, 0 );
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
* @returns {void}
*/
function main() {
	var min;
	var max;
	var N;
	var f;
	var i;
	min = 1;
	max = 3;
	for ( i = min; i <= max; i++ ) {
		N = pow( 10, i );
		f = createBenchmark( N );
		bench( format( '%s:ndarray:N=%d', pkg, N ), f );
	}
}

main();
