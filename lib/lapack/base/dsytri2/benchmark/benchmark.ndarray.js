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
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dsytri2 = require( './../lib/ndarray.js' );


// VARIABLES //

// Block size threshold; matches the dispatch constant in `lib/base.js`.
var NBMAX = 32;


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} N - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( N ) {
	var ldwork;
	var work;
	var ipiv;
	var A;
	var i;

	ipiv = new Int32Array( N );
	if ( NBMAX >= N ) {
		work = new Float64Array( N );
	} else {
		ldwork = N + NBMAX + 1;
		work = new Float64Array( ldwork * ( NBMAX + 3 ) );
	}
	A = new Float64Array( N * N );
	for ( i = 0; i < N; i++ ) {
		A[ i + ( i * N ) ] = 1.0;
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
			A[ 0 ] = 1.0 + ( j * 1e-12 );
			y = dsytri2( 'lower', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
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
		bench( format( '%s:ndarray:N=%d', pkg, len ), f );
	}
}

main();
