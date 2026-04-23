
'use strict';

/**
* Solves a real tridiagonal system using LU factorization from dgttrf (unblocked).
*
* @module @stdlib/lapack/base/dgtts2
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

var dgtts2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtts2 = main;
} else {
	dgtts2 = tmp;
}


// EXPORTS //

module.exports = dgtts2;

// exports: { "ndarray": "dgtts2.ndarray" }
