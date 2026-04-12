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
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsytrf = require( './../../zsytrf/lib/base.js' );
var pkg = require( './../package.json' ).name;
var zla_syrcond_x = require( './../lib/ndarray.js' );


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
	var view;
	var AF;
	var A;
	var X;
	var i;

	data = uniform( 2 * N * N, -1.0, 1.0, options );
	A = new Complex128Array( data.buffer.slice() );
	view = reinterpret( A, 0 );
	for ( i = 0; i < N; i++ ) {
		view[ 2 * ( ( i * N ) + i ) ] += ( 2.0 * N ) + 1.0;
	}
	AF = new Complex128Array( A );
	IPIV = new Int32Array( N );
	zsytrf( 'upper', N, AF, 1, N, 0, IPIV, 1, 0 );

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
			y = zla_syrcond_x( 'upper', N, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, X, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
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

	min = 1; // 10^min
	max = 2; // 10^max

	for ( i = min; i <= max; i++ ) {
		N = pow( 10, i );
		f = createBenchmark( N );
		bench( format( '%s:ndarray:N=%d', pkg, N ), f );
	}
}

main();
