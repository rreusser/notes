
'use strict';

/**
* Solves a triangular banded system with scaling for overflow.
*
* @module @stdlib/lapack/base/dlatbs
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

var dlatbs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlatbs = main;
} else {
	dlatbs = tmp;
}


// EXPORTS //

module.exports = dlatbs;

// exports: { "ndarray": "dlatbs.ndarray" }
