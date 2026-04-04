
'use strict';

/**
* Computes the inverse of a complex Hermitian positive definite matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/zpftri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpftri = require( '@stdlib/lapack/base/zpftri' );
*
* // 3x3 HPD matrix (already Cholesky-factored) in RFP format (TRANSR='N', UPLO='L'):
* // (This would normally be the output of zpftrf.)
* var A = new Complex128Array( [ 3.16227766, 0, 0.94868329, -0.31622776, 0.26726124, 0.77151674, 1.64957219, 0, 2.68328157, 0, 0.62469504, -0.59160797 ] );
* var info = zpftri( 'no-transpose', 'lower', 3, A, 1, 0 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpftri;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpftri = main;
} else {
	zpftri = tmp;
}


// EXPORTS //

module.exports = zpftri;

// exports: { "ndarray": "zpftri.ndarray" }
