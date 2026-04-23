
'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix using MRRR.
*
* @module @stdlib/lapack/base/zheevr
*
* @example
* // TODO: Add example
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zheevr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zheevr = main;
} else {
	zheevr = tmp;
}


// EXPORTS //

module.exports = zheevr;

// exports: { "ndarray": "zheevr.ndarray" }
