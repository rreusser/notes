
'use strict';

/**
* Computes eigenvalues and Schur form of an upper Hessenberg matrix (small/medium).
*
* @module @stdlib/lapack/base/dlahqr
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

var dlahqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlahqr = main;
} else {
	dlahqr = tmp;
}


// EXPORTS //

module.exports = dlahqr;

// exports: { "ndarray": "dlahqr.ndarray" }
