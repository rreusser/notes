
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem.
*
* @module @stdlib/lapack/base/dsbgvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsbgvx = require( '@stdlib/lapack/base/dsbgvx' );
*
* // 5x5 band matrix A (KA=2), upper storage, column-major:
* var AB = new Float64Array( [ 0, 0, 10, 0, 1, 8, 0.5, 2, 6, 0.3, 1.5, 9, 0.4, 1, 7 ] );
* // 5x5 band matrix B (KB=1), upper storage, column-major:
* var BB = new Float64Array( [ 0, 4, 0.2, 5, 0.3, 3, 0.1, 6, 0.2, 4 ] );
* var Q = new Float64Array( 25 );
* var W = new Float64Array( 5 );
* var Z = new Float64Array( 25 );
* var WORK = new Float64Array( 50 );
* var IWORK = new Int32Array( 30 );
* var IFAIL = new Int32Array( 5 );
* var out = { M: 0 };
*
* dsbgvx.ndarray( 'compute-vectors', 'all', 'upper', 5, 2, 1, AB, 1, 3, 0, BB, 1, 2, 0, Q, 1, 5, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 5, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
* // out.M => 5
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dsbgvx.ndarray" }
