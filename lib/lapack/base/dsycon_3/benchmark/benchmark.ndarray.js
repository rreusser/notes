/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dsycon3 = require( './../lib/ndarray.js' );


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
	var rcond;
	var iwork;
	var ipiv;
	var work;
	var A;
	var e;
	var i;

	A = uniform( N * N, -1.0, 1.0, options );
	for ( i = 0; i < N; i += 1 ) {
		A[ ( i * N ) + i ] = ( N * 2 ) + 1;
	}
	e = new Float64Array( N );
	ipiv = new Int32Array( N );
	for ( i = 0; i < N; i += 1 ) {
		ipiv[ i ] = i;
	}
	work = new Float64Array( 2 * N );
	iwork = new Int32Array( N );
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
		var k;

		b.tic();
		for ( k = 0; k < b.iterations; k += 1 ) {
			info = dsycon3( 'upper', N, A, 1, N, 0, e, 1, 0, ipiv, 1, 0, 1.0, rcond, work, 1, 0, iwork, 1, 0 );
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

	min = 1; // 10^min
	max = 2; // 10^max

	for ( i = min; i <= max; i += 1 ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:N=%d', pkg, len ), f );
	}
}

main();
