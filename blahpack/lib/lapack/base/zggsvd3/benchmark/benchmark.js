

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zggsvd3 = require( './../lib/zggsvd3.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - array length
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var N = len;
	var A = uniform( N * N, -10.0, 10.0, options );
	var B = uniform( N * N, -10.0, 10.0, options );
	var ALPHA = uniform( N * N, -10.0, 10.0, options );
	var BETA = uniform( N * N, -10.0, 10.0, options );
	var U = uniform( N * N, -10.0, 10.0, options );
	var V = uniform( N * N, -10.0, 10.0, options );
	var Q = uniform( N * N, -10.0, 10.0, options );
	var WORK = uniform( N * N, -10.0, 10.0, options );
	var RWORK = uniform( N * N, -10.0, 10.0, options );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var y;
		var i;

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			y = zggsvd3( 'row-major', N, N, N, N, N, N, N, N, A, N, B, N, ALPHA, N, BETA, N, U, N, V, N, Q, N, WORK, N, N, RWORK, N, N, N, N );
			if ( isnan( y ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( y ) ) {
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
	max = 6; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:len=%d', pkg, len ), f );
	}
}

main();
