
'use strict';

/**
* Computes the LU factorization of a real tridiagonal matrix.
*
* @module @stdlib/lapack/base/dgttrf
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

var dgttrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgttrf = main;
} else {
	dgttrf = tmp;
}


// EXPORTS //

module.exports = dgttrf;

// exports: { "ndarray": "dgttrf.ndarray" }
