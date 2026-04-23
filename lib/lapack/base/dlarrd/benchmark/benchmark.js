

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var pkg = require( './../package.json' ).name;
var dlarrd = require( './../lib/dlarrd.js' );


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
	var GERS = uniform( N, -10.0, 10.0, options );
	var d = uniform( N, -10.0, 10.0, options );
	var e = uniform( N, -10.0, 10.0, options );
	var E2 = uniform( N, -10.0, 10.0, options );
	var w = uniform( N, -10.0, 10.0, options );
	var WERR = uniform( N, -10.0, 10.0, options );
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
			y = dlarrd( N, 'row-major', N, N, N, N, N, GERS, N, N, d, N, e, N, E2, N, N, N, N, N, N, N, w, N, WERR, N, N, N, N, N, N, N, N, N, WORK, N, N, N, N );
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
