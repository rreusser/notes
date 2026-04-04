

'use strict';

/**
* Equilibrate a Hermitian matrix using scaling factors
*
* @module @stdlib/lapack/base/zlaqhe
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

var zlaqhe;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqhe = main;
} else {
	zlaqhe = tmp;
}


// EXPORTS //

module.exports = zlaqhe;

// exports: { "ndarray": "zlaqhe.ndarray" }
