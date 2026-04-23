'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var pkg = require( './../package.json' ).name;
var zlasyfRk = require( './../lib/zlasyf_rk.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var IPIV;
	var nb;
	var Ar;
	var A;
	var e;
	var W;
	var i;
	nb = Math.min( N, 32 );
	A = new Complex128Array( N * N );
	Ar = reinterpret( A, 0 );

	// Fill with a symmetric-ish pattern (dominant diagonal).
	for ( i = 0; i < N; i++ ) {
		Ar[ ( i * N * 2 ) + ( i * 2 ) ] = 10.0 + i;
	}
	e = new Complex128Array( N );
	IPIV = new Int32Array( N );
	W = new Complex128Array( N * nb );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var ii;
		var y;

		b.tic();
		for ( ii = 0; ii < b.iterations; ii++ ) {
			y = zlasyfRk( 'column-major', 'lower', N, nb, A, N, e, IPIV, W, N );
			if ( y.info !== 0 && y.info < 0 ) {
				b.fail( 'should not return a negative info' );
			}
		}
		b.toc();
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
		bench( format( '%s:N=%d', pkg, N ), f );
	}
}

main();
