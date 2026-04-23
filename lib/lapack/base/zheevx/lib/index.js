

'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix
*
* @module @stdlib/lapack/base/zheevx
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

var zheevx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zheevx = main;
} else {
	zheevx = tmp;
}


// EXPORTS //

module.exports = zheevx;

// exports: { "ndarray": "zheevx.ndarray" }
