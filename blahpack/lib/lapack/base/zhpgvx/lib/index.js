'use strict';

/**
* Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.
*
* @module @stdlib/lapack/base/zhpgvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhpgvx = require( '@stdlib/lapack/base/zhpgvx' );
*
* // A = [4 1-i; 1+i 5] in upper packed storage (complex interleaved):
* var AP = new Complex128Array( [ 4, 0, 1, -1, 5, 0 ] );
*
* // B = [2 0; 0 3] in upper packed storage:
* var BP = new Complex128Array( [ 2, 0, 0, 0, 3, 0 ] );
*
* var w = new Float64Array( 2 );
* var Z = new Complex128Array( 4 );
* var WORK = new Complex128Array( 10 );
* var RWORK = new Float64Array( 20 );
* var IWORK = new Int32Array( 15 );
* var IFAIL = new Int32Array( 2 );
* var out = { 'M': 0 };
*
* var info = zhpgvx( 'column-major', 1, 'compute-vectors', 'all', 'upper', 2, AP, BP, 0, 0, 0, 0, 0, out, w, Z, 2, WORK, RWORK, IWORK, IFAIL );
* // info => 0
* // out.M => 2
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhpgvx.ndarray" }
