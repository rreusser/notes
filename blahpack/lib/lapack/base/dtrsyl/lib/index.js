
'use strict';

/**
* Solves the real Sylvester matrix equation.
*
* @module @stdlib/lapack/base/dtrsyl
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

var dtrsyl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrsyl = main;
} else {
	dtrsyl = tmp;
}


// EXPORTS //

module.exports = dtrsyl;

// exports: { "ndarray": "dtrsyl.ndarray" }
