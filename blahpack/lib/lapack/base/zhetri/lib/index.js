
'use strict';

/**
* Computes the inverse of a complex Hermitian matrix using the factorization computed by zhetrf.
*
* @module @stdlib/lapack/base/zhetri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhetri = require( '@stdlib/lapack/base/zhetri' );
*
* var A = new Complex128Array( [ 4.0, 0.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var WORK = new Complex128Array( 1 );
*
* var info = zhetri.ndarray( 'upper', 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhetri.ndarray" }
