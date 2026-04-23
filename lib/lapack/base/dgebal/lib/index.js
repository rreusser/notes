
'use strict';

/**
* Balances a general real matrix for eigenvalue computation.
*
* @module @stdlib/lapack/base/dgebal
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

var dgebal;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgebal = main;
} else {
	dgebal = tmp;
}


// EXPORTS //

module.exports = dgebal;

// exports: { "ndarray": "dgebal.ndarray" }
