
'use strict';

/**
* Solves the generalized Sylvester equation (blocked).
*
* @module @stdlib/lapack/base/dtgsyl
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

var dtgsyl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtgsyl = main;
} else {
	dtgsyl = tmp;
}


// EXPORTS //

module.exports = dtgsyl;

// exports: { "ndarray": "dtgsyl.ndarray" }
