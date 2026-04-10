

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var ztrsna = require( './../lib/ztrsna.js' );


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
	var N = len;
	var SELECT = uniform( N * N, -10.0, 10.0, options );
	var T = uniform( N * N, -10.0, 10.0, options );
	var VL = uniform( N * N, -10.0, 10.0, options );
	var VR = uniform( N * N, -10.0, 10.0, options );
	var s = uniform( N * N, -10.0, 10.0, options );
	var SEP = uniform( N * N, -10.0, 10.0, options );
	var WORK = uniform( N * N, -10.0, 10.0, options );
	var RWORK = uniform( N * N, -10.0, 10.0, options );
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
			y = ztrsna( 'row-major', N, N, SELECT, N, N, T, N, VL, N, VR, N, s, N, SEP, N, N, N, WORK, N, RWORK, N );
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
	max = 6; // 10^max

	for ( i = min; i <= max; i++ ) {
		len = pow( 10, i );
		f = createBenchmark( len );
		bench( format( '%s:len=%d', pkg, len ), f );
	}
}

main();
