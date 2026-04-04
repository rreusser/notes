
'use strict';

/**
* Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices.
*
* @module @stdlib/lapack/base/dlasy2
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

var dlasy2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasy2 = main;
} else {
	dlasy2 = tmp;
}


// EXPORTS //

module.exports = dlasy2;

// exports: { "ndarray": "dlasy2.ndarray" }
