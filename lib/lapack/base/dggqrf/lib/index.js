
'use strict';

/**
* Computes a generalized QR factorization of matrices A and B.
*
* @module @stdlib/lapack/base/dggqrf
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

var dggqrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dggqrf = main;
} else {
	dggqrf = tmp;
}


// EXPORTS //

module.exports = dggqrf;

// exports: { "ndarray": "dggqrf.ndarray" }
