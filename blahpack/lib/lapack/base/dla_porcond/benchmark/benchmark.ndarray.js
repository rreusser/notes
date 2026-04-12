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

/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Int32Array = require( '@stdlib/array/int32' );
var dpotrf = require( './../../dpotrf/lib/base.js' );
var pkg = require( './../package.json' ).name;
var dla_porcond = require( './../lib/ndarray.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - matrix order
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var IWORK;
	var WORK;
	var AF;
	var N;
	var A;
	var C;
	var i;

	N = len;

	// Create a diagonally dominant SPD matrix:
	A = uniform( N * N, -1.0, 1.0, options );
	for ( i = 0; i < N; i++ ) {
		A[ ( i * N ) + i ] = N + 1.0;
	}
	AF = new Float64Array( A ); // eslint-disable-line stdlib/require-globals
	dpotrf( 'upper', N, AF, 1, N, 0 );
	C = uniform( N, 0.1, 10.0, options );
	WORK = new Float64Array( 3 * N ); // eslint-disable-line stdlib/require-globals
	IWORK = new Int32Array( N );
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
			y = dla_porcond( 'upper', N, A, 1, N, 0, AF, 1, N, 0, 1, C, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
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
	max = 2; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:ndarray:len=%d', pkg, len ), f );
	}
}

main();
