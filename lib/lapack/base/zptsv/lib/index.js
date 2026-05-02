

'use strict';

/**
* Solves a complex Hermitian positive definite tridiagonal system of linear equations
*
* @module @stdlib/lapack/base/zptsv
*
*
* @example
* var zptsv = require( '@stdlib/lapack/base/zptsv' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* zptsv.ndarray( N, N, d, 1, 0, e, 1, 0, B, N, 1, 0 );
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
