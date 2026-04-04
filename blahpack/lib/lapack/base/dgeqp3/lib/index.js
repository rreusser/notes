
'use strict';

/**
* Computes a QR factorization with column pivoting of a real matrix.
*
* @module @stdlib/lapack/base/dgeqp3
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

var dgeqp3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeqp3 = main;
} else {
	dgeqp3 = tmp;
}


// EXPORTS //

module.exports = dgeqp3;

// exports: { "ndarray": "dgeqp3.ndarray" }
