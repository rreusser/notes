
'use strict';

/**
* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix supplied in packed storage.
*
* @module @stdlib/lapack/base/zlansp
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlansp = require( '@stdlib/lapack/base/zlansp' );
*
* var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlansp( 'max', 'upper', 3, AP, WORK );
* // returns ~5.099
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlansp = require( '@stdlib/lapack/base/zlansp' );
*
* var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlansp.ndarray( 'max', 'upper', 3, AP, 1, 0, WORK, 1, 0 );
* // returns ~5.099
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlansp.ndarray" }
