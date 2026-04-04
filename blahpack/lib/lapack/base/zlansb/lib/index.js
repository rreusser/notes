'use strict';

/**
* Returns the norm of a complex symmetric band matrix.
*
* @module @stdlib/lapack/base/zlansb
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlansb = require( '@stdlib/lapack/base/zlansb' );
*
* var AB = new Complex128Array( [ 0, 0, 2, 1, -3, 1, 4, -2, 1, 2, -5, 1, 6, -3, 7, 2 ] );
* var WORK = new Float64Array( 4 );
*
* var result = zlansb( 'max', 'upper', 4, 1, AB, 2, WORK );
* // returns ~7.28
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlansb.ndarray" }
