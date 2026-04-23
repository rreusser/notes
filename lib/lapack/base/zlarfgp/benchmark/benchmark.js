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
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var zlarfgp = require( './../lib/zlarfgp.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - reflector order N
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var x0 = uniform( 2 * ( len - 1 ), -10.0, 10.0, options );
	var x = new Complex128Array( len - 1 );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var alpha;
		var tau;
		var av;
		var xv;
		var tv;
		var i;

		alpha = new Complex128Array( 1 );
		tau = new Complex128Array( 1 );
		av = reinterpret( alpha, 0 );
		xv = reinterpret( x, 0 );
		tv = reinterpret( tau, 0 );

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			// Reset inputs each iteration to avoid operating on already-reflected data:
			av[ 0 ] = 2.0;
			av[ 1 ] = 1.0;
			xv.set( x0 );
			zlarfgp( len, alpha, 0, x, 1, tau, 0 );
			if ( isnan( tv[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( tv[ 0 ] ) ) {
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
	max = 6; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:len=%d', pkg, len ), f );
	}
}

main();
