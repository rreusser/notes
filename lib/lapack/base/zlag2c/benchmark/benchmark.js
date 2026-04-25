/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Complex128Array = require( '@stdlib/array/complex128' );
var uniform = require( '@stdlib/random/array/uniform' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zlag2c = require( './../lib/zlag2c.js' );


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
	var Arr = uniform( 2 * N * N, -10.0, 10.0, options );
	var A = new Complex128Array( Arr.buffer );
	var SA = new Complex128Array( N * N );
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
			info = zlag2c( 'column-major', N, N, A, N, SA, N );
			if ( info !== 0 ) {
				b.fail( 'unexpected info' );
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
	var sizes;
	var i;

	sizes = [ 4, 16, 64, 256 ];
	for ( i = 0; i < sizes.length; i++ ) {
		bench( format( '%s:N=%d', pkg, sizes[ i ] ), createBenchmark( sizes[ i ] ) );
	}
}

main();
