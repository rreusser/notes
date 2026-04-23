/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var pkg = require( './../package.json' ).name;
var dla_lin_berr = require( './../lib/dla_lin_berr.js' );


// VARIABLES //

var options = {
	'dtype': 'float64'
};


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - vector length (N, with nrhs=1)
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var nrhs = 1;
	var berr = new Float64Array( nrhs );
	var res = uniform( len * nrhs, -10.0, 10.0, options );
	var ayb = uniform( len * nrhs, 0.1, 10.0, options );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var i;

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			dla_lin_berr( len, len, nrhs, res, len, ayb, len, berr );
			if ( isnan( berr[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( berr[ 0 ] ) ) {
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
