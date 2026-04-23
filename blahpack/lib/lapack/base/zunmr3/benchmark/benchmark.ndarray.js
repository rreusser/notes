'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Complex128Array = require( '@stdlib/array/complex128' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zunmr3 = require( './../lib/ndarray.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - matrix size
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var WORK;
	var TAU;
	var A;
	var C;
	var N;

	N = len;
	A = new Complex128Array( N * N );
	TAU = new Complex128Array( N );
	C = new Complex128Array( N * N );
	WORK = new Complex128Array( N );
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
			y = zunmr3( 'left', 'no-transpose', N, N, N, 0, A, 1, N, 0, TAU, 1, 0, C, 1, N, 0, WORK, 1, 0 ); // eslint-disable-line max-len
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
	max = 3; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:len=%d', pkg, len ), f );
	}
}

main();
