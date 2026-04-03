
'use strict';

/**
* Computes a contribution to the reciprocal Dif-estimate using the LU factorization computed by zgetc2.
*
* @module @stdlib/lapack/base/zlatdf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
*
* var Z = new Complex128Array( [ 4.0, 1.0, 2.0, 0.5, 3.0, -1.0, 1.0, 2.0 ] );
* var RHS = new Complex128Array( [ 1.0, 0.5, -1.0, 1.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var JPIV = new Int32Array( [ 0, 1 ] );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlatdf.ndarray" }
