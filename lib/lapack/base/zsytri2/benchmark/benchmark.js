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
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zsytri2 = require( './../lib/zsytri2.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var ipiv;
	var A;
	var i;

	ipiv = new Int32Array( N );
	A = new Complex128Array( N * N );
	for ( i = 0; i < N; i++ ) {
		A.set( [ 1.0, 0.0 ], i + ( i * N ) );
		ipiv[ i ] = i;
	}
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var y;
		var j;

		b.tic();
		for ( j = 0; j < b.iterations; j++ ) {
			A.set( [ 1.0 + ( j * 1e-12 ), 0.0 ], 0 );
			y = zsytri2( 'column-major', 'lower', N, A, N, ipiv );
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
	var f;
	var i;

	for ( i = 1; i <= 2; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:N=%d', pkg, len ), f );
	}
}

main();
