

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dlarrf = require( './../lib/dlarrf.js' );


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
	var d = uniform( N, -10.0, 10.0, options );
	var l = uniform( N, -10.0, 10.0, options );
	var LD = uniform( N, -10.0, 10.0, options );
	var w = uniform( N, -10.0, 10.0, options );
	var WGAP = uniform( N, -10.0, 10.0, options );
	var WERR = uniform( N, -10.0, 10.0, options );
	var DPLUS = uniform( N, -10.0, 10.0, options );
	var LPLUS = uniform( N, -10.0, 10.0, options );
	var WORK = uniform( N, -10.0, 10.0, options );
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
			y = dlarrf( N, d, N, l, N, LD, N, N, N, w, N, WGAP, N, WERR, N, N, N, N, N, N, DPLUS, N, LPLUS, N, WORK, N );
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
