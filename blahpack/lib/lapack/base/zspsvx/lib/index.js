

'use strict';

/**
* Solves a complex system A * X = B where A is symmetric in packed storage, with condition estimation and error bounds.
*
* @module @stdlib/lapack/base/zspsvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zspsvx = require( '@stdlib/lapack/base/zspsvx' );
*
* var AP = new Complex128Array( [ 4, 1, 2, -1, 5, 0.5, 1, 2, 3, -1, 6, 1 ] );
* var AFP = new Complex128Array( 6 );
* var IPIV = new Int32Array( 3 );
* var B = new Complex128Array( [ 7, 2, 10, -1.5, 10, 2 ] );
* var X = new Complex128Array( 3 );
* var rcond = new Float64Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = zspsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zspsvx.ndarray" }
