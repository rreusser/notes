
'use strict';

/**
* Equilibrates a general matrix using row and column scaling factors.
*
* @module @stdlib/lapack/base/dlaqge
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

var dlaqge;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqge = main;
} else {
	dlaqge = tmp;
}


// EXPORTS //

module.exports = dlaqge;

// exports: { "ndarray": "dlaqge.ndarray" }
