

'use strict';

/**
* Computes eigenvectors of a real symmetric tridiagonal matrix by inverse iteration
*
* @module @stdlib/lapack/base/zstein
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

var zstein;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zstein = main;
} else {
	zstein = tmp;
}


// EXPORTS //

module.exports = zstein;

// exports: { "ndarray": "zstein.ndarray" }
