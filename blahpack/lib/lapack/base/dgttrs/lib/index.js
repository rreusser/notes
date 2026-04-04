
'use strict';

/**
* Solves a real tridiagonal system using LU factorization from dgttrf.
*
* @module @stdlib/lapack/base/dgttrs
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

var dgttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgttrs = main;
} else {
	dgttrs = tmp;
}


// EXPORTS //

module.exports = dgttrs;

// exports: { "ndarray": "dgttrs.ndarray" }
