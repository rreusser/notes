
'use strict';

/**
* Solves a system of linear equations using the LU factorization with complete pivoting computed by zgetc2.
*
* @module @stdlib/lapack/base/zgesc2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zgesc2 = require( '@stdlib/lapack/base/zgesc2' );
*
* var A = new Complex128Array( [ 4.0, 1.0, 0.353, 0.412, 2.0, -1.0, 1.882, 0.029 ] );
* var RHS = new Complex128Array( [ 10.0, 3.0, 7.0, 4.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var JPIV = new Int32Array( [ 0, 1 ] );
* var scale = new Float64Array( 1 );
*
* zgesc2.ndarray( 2, A, 1, 2, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );
* // RHS is overwritten with the solution
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zgesc2.ndarray" }
