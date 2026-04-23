
/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dla_geamv = require( './../lib/dla_geamv.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - array length
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var y0;
	var N;
	var A;
	var x;

	N = len;
	A = uniform( N * N, -10.0, 10.0, options );
	x = uniform( N, -10.0, 10.0, options );
	y0 = uniform( N, -10.0, 10.0, options );
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

		y = y0;
		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			y = dla_geamv( 'row-major', 'no-transpose', N, N, 1.0, A, N, x, 1, 1.0, y, 1 );
			if ( isnan( y[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( y[ 0 ] ) ) {
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
	max = 3; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:len=%d', pkg, len ), f );
	}
}

main();
