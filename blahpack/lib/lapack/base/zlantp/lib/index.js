
'use strict';

/**
* Returns the norm of a complex triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/zlantp
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlantp = require( '@stdlib/lapack/base/zlantp' );
*
* var AP = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 7.0, 8.0, 5.0, 6.0, 9.0, 1.0, 2.0, 3.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlantp( 'max', 'upper', 'non-unit', 3, AP, WORK );
* // returns ~10.63
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlantp = require( '@stdlib/lapack/base/zlantp' );
*
* var AP = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 7.0, 8.0, 5.0, 6.0, 9.0, 1.0, 2.0, 3.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlantp.ndarray( 'max', 'upper', 'non-unit', 3, AP, 1, 0, WORK, 1, 0 );
* // returns ~10.63
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlantp.ndarray" }
