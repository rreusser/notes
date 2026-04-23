/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgetrf = require( './../../zgetrf/lib/base.js' );
var pkg = require( './../package.json' ).name;
var zla_gercond_x = require( './../lib/ndarray.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var RWORK;
	var IPIV;
	var WORK;
	var data;
	var AF;
	var A;
	var X;

	data = uniform( 2 * N * N, -1.0, 1.0, options );
	A = new Complex128Array( data.buffer.slice() );
	AF = new Complex128Array( A );
	IPIV = new Int32Array( N );
	zgetrf( N, N, AF, 1, N, 0, IPIV, 1, 0 );

	X = new Complex128Array( uniform( 2 * N, 0.5, 1.5, options ).buffer.slice() );
	WORK = new Complex128Array( 2 * N );
	RWORK = new Float64Array( N );

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
			y = zla_gercond_x( 'no-transpose', N, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, X, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
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
	max = 2; // 10^max  (N=100)

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:len=%d', pkg, len ), f );
	}
}

main();
