
'use strict';

/**
* Returns the norm of a complex triangular band matrix.
*
* @module @stdlib/lapack/base/zlantb
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlantb = require( '@stdlib/lapack/base/zlantb' );
*
* var AB = new Complex128Array( [ 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5 ] );
* var WORK = new Float64Array( 3 );
*
* var v = zlantb.ndarray( 'one-norm', 'upper', 'non-unit', 3, 1, AB, 1, 2, 0, WORK, 1, 0 );
* // returns ~12.73
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlantb.ndarray" }
