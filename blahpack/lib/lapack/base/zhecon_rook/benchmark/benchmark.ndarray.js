'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zheconRook = require( './../lib/ndarray.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var rcond;
	var work;
	var ipiv;
	var Av;
	var A;
	var i;
	A = new Complex128Array( N * N );
	Av = new Float64Array( A.buffer );
	for ( i = 0; i < N; i++ ) {
		Av[ 2 * ((i*N) + i) ] = N + 1.0;
	}
	ipiv = new Int32Array( N );
	for ( i = 0; i < N; i++ ) {
		ipiv[ i ] = i + 1;
	}
	work = new Complex128Array( 2 * N );
	rcond = new Float64Array( 1 );
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
			info = zheconRook( 'upper', N, A, 1, N, 0, ipiv, 1, 0, 1.0, rcond, work, 1, 0 ); // eslint-disable-line max-len
			if ( isnan( rcond[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( info !== 0 ) {
			b.fail( 'unexpected info' );
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

	min = 1;
	max = 2;

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:len=%d', pkg, len ), f );
	}
}

main();
