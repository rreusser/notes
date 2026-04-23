

'use strict';

/**
* Complex Hermitian indefinite expert solver
*
* @module @stdlib/lapack/base/zhesvx
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

var zhesvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhesvx = main;
} else {
	zhesvx = tmp;
}


// EXPORTS //

module.exports = zhesvx;

// exports: { "ndarray": "zhesvx.ndarray" }
