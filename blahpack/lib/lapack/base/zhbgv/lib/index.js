
'use strict';

/**
* Compute all eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.
*
* @module @stdlib/lapack/base/zhbgv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
* var zhbgv = require( '@stdlib/lapack/base/zhbgv' );
*
* // 3x3 diagonal (KA=KB=0):
* var AB = new Complex128Array( 3 );
* var BB = new Complex128Array( 3 );
* var v = reinterpret( AB, 0 );
* v[ 0 ] = 5.0; v[ 2 ] = 6.0; v[ 4 ] = 7.0;
* v = reinterpret( BB, 0 );
* v[ 0 ] = 2.0; v[ 2 ] = 3.0; v[ 4 ] = 4.0;
* var W = new Float64Array( 3 );
* var Z = new Complex128Array( 9 );
* var WORK = new Complex128Array( 3 );
* var RWORK = new Float64Array( 9 );
*
* zhbgv.ndarray( 'compute-vectors', 'upper', 3, 0, 0, AB, 1, 1, 0, BB, 1, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhbgv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhbgv = main;
} else {
	zhbgv = tmp;
}


// EXPORTS //

module.exports = zhbgv;

// exports: { "ndarray": "zhbgv.ndarray" }
