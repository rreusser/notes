
'use strict';

/**
* Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian matrix in packed storage.
*
* @module @stdlib/lapack/base/zhpevx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zhpevx = require( '@stdlib/lapack/base/zhpevx' );
*
* // 2x2 Hermitian [[3,1-i],[1+i,1]] upper packed: [(3,0), (1,-1), (1,0)]
* var AP = new Complex128Array( [ 3, 0, 1, -1, 1, 0 ] );
* var w = new Float64Array( 2 );
* var Z = new Complex128Array( 4 );
* var WORK = new Complex128Array( 8 );
* var RWORK = new Float64Array( 32 );
* var IWORK = new Int32Array( 16 );
* var IFAIL = new Int32Array( 2 );
* var out = { M: 0 };
*
* zhpevx( 'column-major', 'compute-vectors', 'all', 'upper', 2, AP, 0, 0, 0, 0, 0, out, w, Z, 2, WORK, RWORK, IWORK, IFAIL );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zhpevx = require( '@stdlib/lapack/base/zhpevx' );
*
* // 2x2 Hermitian [[3,1-i],[1+i,1]] upper packed: [(3,0), (1,-1), (1,0)]
* var AP = new Complex128Array( [ 3, 0, 1, -1, 1, 0 ] );
* var w = new Float64Array( 2 );
* var Z = new Complex128Array( 4 );
* var WORK = new Complex128Array( 8 );
* var RWORK = new Float64Array( 32 );
* var IWORK = new Int32Array( 16 );
* var IFAIL = new Int32Array( 2 );
* var out = { M: 0 };
*
* zhpevx.ndarray( 'compute-vectors', 'all', 'upper', 2, AP, 1, 0, 0, 0, 0, 0, 0, out, w, 1, 0, Z, 1, 2, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhpevx.ndarray" }
