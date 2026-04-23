
'use strict';

/**
* Apply a vector of real plane rotations to two vectors.
*
* @module @stdlib/lapack/base/dlartv
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

var dlartv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlartv = main;
} else {
	dlartv = tmp;
}


// EXPORTS //

module.exports = dlartv;

// exports: { "ndarray": "dlartv.ndarray" }
