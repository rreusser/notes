/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dlarfb_gett = require( './../lib/dlarfb_gett.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - problem size
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var WORK;
	var K;
	var M;
	var N;
	var T;
	var A;
	var B;

	K = len;
	M = len;
	N = 2 * len;
	T = uniform( K * K, -10.0, 10.0, options );
	A = uniform( K * N, -10.0, 10.0, options );
	B = uniform( M * N, -10.0, 10.0, options );
	WORK = uniform( K * N, -10.0, 10.0, options );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var i;

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			dlarfb_gett( 'column-major', 'not-identity', M, N, K, T, K, A, K, B, M, WORK, K );
			if ( isnan( A[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( A[ 0 ] ) ) {
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
	max = 2; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:len=%d', pkg, len ), f );
	}
}

main();
