

'use strict';

/**
* Expert solver for Hermitian positive definite system
*
* @module @stdlib/lapack/base/zposvx
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

var zposvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zposvx = main;
} else {
	zposvx = tmp;
}


// EXPORTS //

module.exports = zposvx;

// exports: { "ndarray": "zposvx.ndarray" }
