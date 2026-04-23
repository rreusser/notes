
'use strict';

/**
* Solve tridiagonal system using LU factorization (complex).
*
* @module @stdlib/lapack/base/zgttrs
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

var zgttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgttrs = main;
} else {
	zgttrs = tmp;
}


// EXPORTS //

module.exports = zgttrs;

// exports: { "ndarray": "zgttrs.ndarray" }
