
'use strict';

/**
* Computes a split Cholesky factorization of a complex Hermitian positive definite band matrix.
*
* @module @stdlib/lapack/base/zpbstf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpbstf = require( '@stdlib/lapack/base/zpbstf' );
*
* var ab = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, -1.0, 0.5, 4.0, 0.0, -1.0, 0.5, 4.0, 0.0, -1.0, 0.5, 4.0, 0.0 ] );
* var info = zpbstf.ndarray( 'upper', 4, 1, ab, 1, 2, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpbstf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpbstf = main;
} else {
	zpbstf = tmp;
}


// EXPORTS //

module.exports = zpbstf;

// exports: { "ndarray": "zpbstf.ndarray" }
