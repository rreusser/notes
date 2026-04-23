/* eslint-disable camelcase */

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var uniform = require( '@stdlib/random/array/uniform' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pow = require( '@stdlib/math/base/special/pow' );
var format = require( '@stdlib/string/format' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var pkg = require( './../package.json' ).name;
var zla_heamv = require( './../lib/ndarray.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @param {PositiveInteger} len - array length
* @returns {Function} benchmark function
*/
function createBenchmark( len ) {
	var opts = {
		'dtype': 'float64'
	};
	var A = new Complex128Array( uniform( 2 * len * len, -10.0, 10.0, opts ) );
	var x = new Complex128Array( uniform( 2 * len, -10.0, 10.0, opts ) );
	var y = new Float64Array( uniform( len, -10.0, 10.0, opts ) );
	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var z;
		var i;

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			z = zla_heamv( 'upper', len, 1.0, A, 1, len, 0, x, 1, 0, 1.0, y, 1, 0 );
			if ( isnan( z[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( z[ 0 ] ) ) {
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
		bench( format( '%s:ndarray:len=%d', pkg, len ), f );
	}
}

main();
