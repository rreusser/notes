
'use strict';

/**
* Converts a symmetric matrix factored by dsytrf to standard L_D_L^T form and vice versa.
*
* @module @stdlib/lapack/base/dsyconv
*
*
* @example
* var dsyconv = require( '@stdlib/lapack/base/dsyconv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var E = discreteUniform( N, -10, 10, opts );
*
* dsyconv.ndarray( 'upper', '1', N, A, N, 1, 0, IPIV, 1, 0, E, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsyconv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsyconv = main;
} else {
	dsyconv = tmp;
}


// EXPORTS //

module.exports = dsyconv;

// exports: { "ndarray": "dsyconv.ndarray" }
