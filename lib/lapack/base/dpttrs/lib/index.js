
'use strict';

/**
* Solves a real symmetric positive definite tridiagonal system using LDL^T factorization.
*
* @module @stdlib/lapack/base/dpttrs
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

var dpttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpttrs = main;
} else {
	dpttrs = tmp;
}


// EXPORTS //

module.exports = dpttrs;

// exports: { "ndarray": "dpttrs.ndarray" }
