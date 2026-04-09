

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zgges = require( './../lib/ndarray.js' );


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
	var VSL = uniform( N * N, -10.0, 10.0, options );
	var VSR = uniform( N * N, -10.0, 10.0, options );
	var WORK = uniform( N * N, -10.0, 10.0, options );
	var RWORK = uniform( N * N, -10.0, 10.0, options );
	var BWORK = uniform( N * N, -10.0, 10.0, options );
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
			y = zgges( N, N, N, N, N, A, N, 1, 0, B, N, 1, 0, N, ALPHA, 1, 0, BETA, 1, 0, VSL, N, 1, 0, VSR, N, 1, 0, WORK, 1, 0, N, RWORK, 1, 0, BWORK, 1, 0 );
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
		bench( format( '%s:ndarray:len=%d', pkg, len ), f );
	}
}

main();
