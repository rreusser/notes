'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var pkg = require( './../package.json' ).name;
var zlaic1 = require( './../lib/ndarray.js' );


// FUNCTIONS //

/**
* Creates a benchmark function.
*
* @private
* @returns {Function} benchmark function
*/
function createBenchmark() {
	var gamma = new Complex128( 1.0, 0.5 );
	var x = new Complex128Array( [ 0.6, 0.1, 0.5, -0.2, 0.4, 0.3 ] );
	var w = new Complex128Array( [ 0.3, 0.4, 0.7, -0.1, 0.2, 0.5 ] );

	return benchmark;

	/**
	* Benchmark function.
	*
	* @private
	* @param {Benchmark} b - benchmark instance
	*/
	function benchmark( b ) {
		var sestpr;
		var sv;
		var cv;
		var i;

		sestpr = new Float64Array( 1 );
		sv = new Float64Array( 2 );
		cv = new Float64Array( 2 );

		b.tic();
		for ( i = 0; i < b.iterations; i++ ) {
			zlaic1( 'largest-singular-value', 3, x, 1, 0, 2.5, w, 1, 0, gamma, sestpr, sv, cv ); // eslint-disable-line max-len
			if ( isnan( sestpr[ 0 ] ) ) {
				b.fail( 'should not return NaN' );
			}
		}
		b.toc();
		if ( isnan( sestpr[ 0 ] ) ) {
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
	var f;

	f = createBenchmark();
	bench( pkg + ':ndarray', f );
}

main();
