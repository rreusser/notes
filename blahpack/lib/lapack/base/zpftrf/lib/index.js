
'use strict';

/**
* Computes the Cholesky factorization of a complex Hermitian positive definite matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/zpftrf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpftrf = require( '@stdlib/lapack/base/zpftrf' );
*
* // 3x3 HPD matrix in RFP format (TRANSR='N', UPLO='L'):
* var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );
* var info = zpftrf( 'no-transpose', 'lower', 3, A, 1, 0 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpftrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpftrf = main;
} else {
	zpftrf = tmp;
}


// EXPORTS //

module.exports = zpftrf;

// exports: { "ndarray": "zpftrf.ndarray" }
