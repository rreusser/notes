
'use strict';

/**
* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex Hermitian matrix supplied in packed storage.
*
* @module @stdlib/lapack/base/zlanhp
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlanhp = require( '@stdlib/lapack/base/zlanhp' );
*
* var AP = new Complex128Array( [ 2.0, 0.0, 1.0, 2.0, 5.0, 0.0, -1.0, 3.0, 0.5, -1.5, 7.0, 0.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlanhp( 'max', 'upper', 3, AP, WORK );
* // returns 7.0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlanhp = require( '@stdlib/lapack/base/zlanhp' );
*
* var AP = new Complex128Array( [ 2.0, 0.0, 1.0, 2.0, 5.0, 0.0, -1.0, 3.0, 0.5, -1.5, 7.0, 0.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlanhp.ndarray( 'max', 'upper', 3, AP, 1, 0, WORK, 1, 0 );
* // returns 7.0
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlanhp.ndarray" }
