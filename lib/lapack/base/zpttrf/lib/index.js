

'use strict';

/**
* Computes the LDL^H factorization of a complex Hermitian positive definite tridiagonal matrix
*
* @module @stdlib/lapack/base/zpttrf
*
*
* @example
* var zpttrf = require( '@stdlib/lapack/base/zpttrf' );
*
* var N = 3;
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* zpttrf.ndarray( N, d, 1, 0, e, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpttrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpttrf = main;
} else {
	zpttrf = tmp;
}


// EXPORTS //

module.exports = zpttrf;

// exports: { "ndarray": "zpttrf.ndarray" }
