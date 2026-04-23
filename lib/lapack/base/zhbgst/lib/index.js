
'use strict';

/**
* Reduces a complex Hermitian-definite banded generalized eigenproblem to standard form.
*
* @module @stdlib/lapack/base/zhbgst
*
* @example
* var zhbgst = require( '@stdlib/lapack/base/zhbgst' );
* // See examples/index.js for usage
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhbgst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhbgst = main;
} else {
	zhbgst = tmp;
}


// EXPORTS //

module.exports = zhbgst;

// exports: { "ndarray": "zhbgst.ndarray" }
