
'use strict';

/**
* Improves the computed solution to a complex system A * X = B where A is symmetric in packed storage and provides error bounds.
*
* @module @stdlib/lapack/base/zsprfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsprfs = require( '@stdlib/lapack/base/zsprfs' );
*
* var AP = new Complex128Array( new Float64Array( [ 3.0, 1.0 ] ) );
* var AFP = new Complex128Array( new Float64Array( [ 3.0, 1.0 ] ) );
* var IPIV = new Int32Array( [ 0 ] );
* var B = new Complex128Array( new Float64Array( [ 1.0, 1.0 ] ) );
* var X = new Complex128Array( new Float64Array( [ 0.4, 0.2 ] ) );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 2 );
* var RWORK = new Float64Array( 1 );
*
* var info = zsprfs.ndarray( 'upper', 1, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zsprfs.ndarray" }
