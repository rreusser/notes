
'use strict';

/**
* Computes contribution to reciprocal DIF estimate using LU factorization from dgetc2.
*
* @module @stdlib/lapack/base/dlatdf
*
*
* @example
* var dlatdf = require( '@stdlib/lapack/base/dlatdf' );
*
* var N = 3;
* var Z = discreteUniform( N * N, -10, 10, opts );
* var RHS = discreteUniform( N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var JPIV = discreteUniform( N, -10, 10, opts );
*
* dlatdf.ndarray( 1, N, Z, N, 1, 0, RHS, 1, 0, 1, 1, IPIV, 1, 0, JPIV, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlatdf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlatdf = main;
} else {
	dlatdf = tmp;
}


// EXPORTS //

module.exports = dlatdf;

// exports: { "ndarray": "dlatdf.ndarray" }
