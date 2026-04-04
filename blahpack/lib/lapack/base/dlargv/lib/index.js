
'use strict';

/**
* Generate a vector of real plane rotations.
*
* @module @stdlib/lapack/base/dlargv
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

var dlargv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlargv = main;
} else {
	dlargv = tmp;
}


// EXPORTS //

module.exports = dlargv;

// exports: { "ndarray": "dlargv.ndarray" }
