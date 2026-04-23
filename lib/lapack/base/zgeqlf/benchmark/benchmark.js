'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Complex128Array = require( '@stdlib/array/complex128' );
var pkg = require( './../package.json' ).name;
var zgeqlf = require( './../lib/zgeqlf.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix dimension
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var WORK = new Complex128Array( N * 64 );
	var TAU = new Complex128Array( N );
	var A = new Complex128Array( uniform( 2 * N * N, -10.0, 10.0, options ) );
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
			y = zgeqlf( 'column-major', N, N, A, N, TAU, 1, WORK, 1, -1 );
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
	var min;
	var max;
	var N;
	var f;
	var i;

	min = 1;
	max = 4; // 2^4 = 16

	for ( i = min; i <= max; i++ ) {
		N = pow( 2, i );
		f = createBenchmark( N );
		bench( format( '%s:N=%d', pkg, N ), f );
	}
}

main();
