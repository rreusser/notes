
'use strict';

/**
* Reduces a real symmetric-definite banded generalized eigenproblem to standard form.
*
* @module @stdlib/lapack/base/dsbgst
*
* @example
* var dsbgst = require( '@stdlib/lapack/base/dsbgst' );
* // See examples/index.js for usage
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsbgst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsbgst = main;
} else {
	dsbgst = tmp;
}


// EXPORTS //

module.exports = dsbgst;

// exports: { "ndarray": "dsbgst.ndarray" }
