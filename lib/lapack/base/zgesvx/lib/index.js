
'use strict';

/**
* Expert driver for solving complex general linear systems.
*
* @module @stdlib/lapack/base/zgesvx
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

var zgesvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgesvx = main;
} else {
	zgesvx = tmp;
}


// EXPORTS //

module.exports = zgesvx;

// exports: { "ndarray": "zgesvx.ndarray" }
