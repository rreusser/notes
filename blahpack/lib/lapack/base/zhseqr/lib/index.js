

'use strict';

/**
* Compute eigenvalues and Schur form of complex upper Hessenberg matrix
*
* @module @stdlib/lapack/base/zhseqr
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

var zhseqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhseqr = main;
} else {
	zhseqr = tmp;
}


// EXPORTS //

module.exports = zhseqr;

// exports: { "ndarray": "zhseqr.ndarray" }
