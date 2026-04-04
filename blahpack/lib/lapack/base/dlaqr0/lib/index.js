

'use strict';

/**
* Computes eigenvalues and Schur form using multishift QR with aggressive early deflation
*
* @module @stdlib/lapack/base/dlaqr0
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

var dlaqr0;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr0 = main;
} else {
	dlaqr0 = tmp;
}


// EXPORTS //

module.exports = dlaqr0;

// exports: { "ndarray": "dlaqr0.ndarray" }
