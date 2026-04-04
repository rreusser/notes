
'use strict';

/**
* Computes the inverse of a complex symmetric matrix using the factorization computed by zsytrf.
*
* @module @stdlib/lapack/base/zsytri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytri = require( '@stdlib/lapack/base/zsytri' );
*
* var A = new Complex128Array( [ 4.0, 1.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var WORK = new Complex128Array( 1 );
*
* var info = zsytri.ndarray( 'upper', 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zsytri.ndarray" }
