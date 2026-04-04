
'use strict';

/**
* Improves the computed solution to a real system A * X = B where A is symmetric in packed storage and provides error bounds.
*
* @module @stdlib/lapack/base/dsprfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
* var dsptrs = require( '@stdlib/lapack/base/dsptrs' );
* var dsprfs = require( '@stdlib/lapack/base/dsprfs' );
*
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var AFP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var IPIV = new Int32Array( 3 );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* dsptrf( 'upper', 3, AFP, 1, IPIV, 1 );
* dsptrs( 'upper', 3, 1, AFP, 1, IPIV, 1, X, 3 );
* dsprfs( 'upper', 3, 1, AP, 1, AFP, 1, IPIV, 1, B, 3, X, 3, FERR, 1, BERR, 1, WORK, 1, IWORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsprfs = require( '@stdlib/lapack/base/dsprfs' );
*
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var AFP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var IPIV = new Int32Array( 3 );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* dsprfs.ndarray( 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dsprfs.ndarray" }
