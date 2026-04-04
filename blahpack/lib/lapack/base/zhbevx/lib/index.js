
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.
*
* @module @stdlib/lapack/base/zhbevx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zhbevx = require( '@stdlib/lapack/base/zhbevx' );
*
* // 4x4 Hermitian tridiagonal (KD=1), lower band storage:
* var AB = new Complex128Array( [ 4, 0, 1, -1, 5, 0, 2, 1, 6, 0, 3, -1, 7, 0, 0, 0 ] );
* var Q = new Complex128Array( 16 );
* var W = new Float64Array( 4 );
* var Z = new Complex128Array( 16 );
* var WORK = new Complex128Array( 20 );
* var RWORK = new Float64Array( 50 );
* var IWORK = new Int32Array( 30 );
* var IFAIL = new Int32Array( 4 );
* var out = { M: 0 };
*
* zhbevx.ndarray( 'compute-vectors', 'all', 'lower', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
* // out.M => 4
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhbevx.ndarray" }
