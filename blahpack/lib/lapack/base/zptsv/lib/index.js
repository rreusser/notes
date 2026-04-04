

'use strict';

/**
* Solves a complex Hermitian positive definite tridiagonal system of linear equations
*
* @module @stdlib/lapack/base/zptsv
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

var zptsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zptsv = main;
} else {
	zptsv = tmp;
}


// EXPORTS //

module.exports = zptsv;

// exports: { "ndarray": "zptsv.ndarray" }
