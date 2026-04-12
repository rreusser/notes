
'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var floor = require( '@stdlib/math/base/special/floor' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dlarzt = require( './../lib/ndarray.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - dimension
* @param {PositiveInteger} K - number of reflectors
* @returns {Function} benchmark function
*/
function createBenchmark( N, K ) {
	var TAU = uniform( K, -1.0, 1.0, options );
	var V = uniform( K * N, -10.0, 10.0, options );
	var T = uniform( K * K, 0.0, 0.0, options );
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
			dlarzt( 'backward', 'rowwise', N, K, V, 1, K, 0, TAU, 1, 0, T, 1, K, 0 );
			if ( isnan( T[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( T[ 0 ] ) ) {
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
	var min;
	var max;
	var K;
	var N;
	var f;
	var i;

	min = 1; // 10^min
	max = 3; // 10^max

	for ( i = min; i <= max; i++ ) {
		N = pow( 10, i );
		K = floor( N / 4 );
		if ( K < 1 ) {
			K = 1;
		}
		f = createBenchmark( N, K );
		bench( format( '%s:ndarray:N=%d,K=%d', pkg, N, K ), f );
	}
}

main();
