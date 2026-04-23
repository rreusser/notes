
'use strict';

/**
* Computes selected eigenvalues and optionally eigenvectors of a real symmetric-definite generalized eigenproblem in packed storage.
*
* @module @stdlib/lapack/base/dspgvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dspgvx = require( '@stdlib/lapack/base/dspgvx' );
*
* // A = [4 2 1; 2 5 3; 1 3 6] in upper packed storage:
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
*
* // B = [4 2 0; 2 5 1; 0 1 3] in upper packed storage:
* var BP = new Float64Array( [ 4.0, 2.0, 5.0, 0.0, 1.0, 3.0 ] );
*
* var W = new Float64Array( 3 );
* var Z = new Float64Array( 9 );
* var WORK = new Float64Array( 24 );
* var IWORK = new Int32Array( 15 );
* var IFAIL = new Int32Array( 3 );
* var out = { 'M': 0 };
*
* var info = dspgvx( 'column-major', 1, 'compute-vectors', 'all', 'upper', 3, AP, BP, 0, 0, 0, 0, 0, out, W, Z, 3, WORK, IWORK, IFAIL );
* // info => 0
* // out.M => 3
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspgvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspgvx = main;
} else {
	dspgvx = tmp;
}


// EXPORTS //

module.exports = dspgvx;

// exports: { "ndarray": "dspgvx.ndarray" }
