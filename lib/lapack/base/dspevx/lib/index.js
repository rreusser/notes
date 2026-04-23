
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.
*
* @module @stdlib/lapack/base/dspevx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dspevx = require( '@stdlib/lapack/base/dspevx' );
*
* // 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
* var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var w = new Float64Array( 2 );
* var Z = new Float64Array( 4 );
* var WORK = new Float64Array( 16 );
* var IWORK = new Int32Array( 10 );
* var IFAIL = new Int32Array( 2 );
* var out = { M: 0 };
*
* dspevx( 'column-major', 'compute-vectors', 'all', 'upper', 2, AP, 0, 0, 0, 0, 0, out, w, Z, 2, WORK, IWORK, IFAIL );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dspevx = require( '@stdlib/lapack/base/dspevx' );
*
* // 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
* var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var w = new Float64Array( 2 );
* var Z = new Float64Array( 4 );
* var WORK = new Float64Array( 16 );
* var IWORK = new Int32Array( 10 );
* var IFAIL = new Int32Array( 2 );
* var out = { M: 0 };
*
* dspevx.ndarray( 'compute-vectors', 'all', 'upper', 2, AP, 1, 0, 0, 0, 0, 0, 0, out, w, 1, 0, Z, 1, 2, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspevx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspevx = main;
} else {
	dspevx = tmp;
}


// EXPORTS //

module.exports = dspevx;

// exports: { "ndarray": "dspevx.ndarray" }
