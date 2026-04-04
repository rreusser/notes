
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric band matrix.
*
* @module @stdlib/lapack/base/dsbevx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsbevx = require( '@stdlib/lapack/base/dsbevx' );
*
* // 4x4 tridiagonal (KD=1), upper band storage, column-major:
* var AB = new Float64Array( [ 0, 4, 1, 5, 2, 6, 3, 7 ] );
* var Q = new Float64Array( 16 );
* var W = new Float64Array( 4 );
* var Z = new Float64Array( 16 );
* var WORK = new Float64Array( 50 );
* var IWORK = new Int32Array( 30 );
* var IFAIL = new Int32Array( 4 );
* var out = { M: 0 };
*
* dsbevx.ndarray( 'compute-vectors', 'all', 'upper', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
* // out.M => 4
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dsbevx.ndarray" }
