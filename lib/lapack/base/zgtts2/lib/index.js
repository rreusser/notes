
'use strict';

/**
* Solve tridiagonal system using LU factorization (complex).
*
* @module @stdlib/lapack/base/zgtts2
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

var zgtts2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtts2 = main;
} else {
	zgtts2 = tmp;
}


// EXPORTS //

module.exports = zgtts2;

// exports: { "ndarray": "zgtts2.ndarray" }
