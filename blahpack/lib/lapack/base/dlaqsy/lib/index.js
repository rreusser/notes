
'use strict';

/**
* Equilibrate a symmetric matrix using scaling factors.
*
* @module @stdlib/lapack/base/dlaqsy
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

var dlaqsy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqsy = main;
} else {
	dlaqsy = tmp;
}


// EXPORTS //

module.exports = dlaqsy;

// exports: { "ndarray": "dlaqsy.ndarray" }
