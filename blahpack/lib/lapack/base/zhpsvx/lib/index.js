
'use strict';

/**
* Solves a complex system A * X = B where A is Hermitian in packed storage, with condition estimation and error bounds.
*
* @module @stdlib/lapack/base/zhpsvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhpsvx = require( '@stdlib/lapack/base/zhpsvx' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 5.0, 0.0, 2.0, -1.0, 3.0, 1.0, 6.0, 0.0 ] );
* var AFP = new Complex128Array( 6 );
* var IPIV = new Int32Array( 3 );
* var B = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0, 1.0, -1.0 ] );
* var X = new Complex128Array( 3 );
* var rcond = new Float64Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = zhpsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhpsvx.ndarray" }
