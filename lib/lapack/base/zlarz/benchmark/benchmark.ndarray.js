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
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zlarz = require( './../lib/ndarray.js' );


// VARIABLES //

var options = {
	'dtype': 'complex128'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - matrix dimension
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var WORK;
	var tau;
	var N;
	var L;
	var C;
	var v;
	N = len;
	L = ( N > 2 ) ? ( N >> 1 ) : 1;
	C = uniform( N * N, -1.0, 1.0, options );
	v = uniform( L, -1.0, 1.0, options );
	tau = uniform( 1, -1.0, 1.0, options );
	WORK = uniform( N, -1.0, 1.0, options );
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
			y = zlarz( 'left', N, N, L, v, 1, 0, tau, 0, C, 1, N, 0, WORK, 1, 0 );
			if ( isnan( y.get( 0 ).re ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( y.get( 0 ).re ) ) {
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
