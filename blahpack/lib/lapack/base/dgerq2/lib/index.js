
'use strict';

/**
* Compute the RQ factorization of a real matrix (unblocked).
*
* @module @stdlib/lapack/base/dgerq2
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

var dgerq2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgerq2 = main;
} else {
	dgerq2 = tmp;
}


// EXPORTS //

module.exports = dgerq2;

// exports: { "ndarray": "dgerq2.ndarray" }
