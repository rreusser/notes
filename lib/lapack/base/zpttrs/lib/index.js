

'use strict';

/**
* Solves a complex Hermitian positive definite tridiagonal system using LDL^H factorization
*
* @module @stdlib/lapack/base/zpttrs
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

var zpttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpttrs = main;
} else {
	zpttrs = tmp;
}


// EXPORTS //

module.exports = zpttrs;

// exports: { "ndarray": "zpttrs.ndarray" }
