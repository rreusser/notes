
'use strict';

/**
* Compute LU factorization of complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zgttrf
*
*
* @example
* var zgttrf = require( '@stdlib/lapack/base/zgttrf' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zgttrf.ndarray( N, DL, 1, 0, d, 1, 0, DU, 1, 0, 1, 1, 0, IPIV, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgttrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgttrf = main;
} else {
	zgttrf = tmp;
}


// EXPORTS //

module.exports = zgttrf;

// exports: { "ndarray": "zgttrf.ndarray" }
