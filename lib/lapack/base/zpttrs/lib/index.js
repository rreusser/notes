

'use strict';

/**
* Solves a complex Hermitian positive definite tridiagonal system using LDL^H factorization
*
* @module @stdlib/lapack/base/zpttrs
*
*
* @example
* var zpttrs = require( '@stdlib/lapack/base/zpttrs' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* zpttrs.ndarray( 'upper', N, N, d, 1, 0, e, 1, 0, B, N, 1, 0 );
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
