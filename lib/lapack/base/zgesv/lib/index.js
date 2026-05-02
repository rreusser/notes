
'use strict';

/**
* Compute the solution to a complex system of linear equations A * X = B.
*
* @module @stdlib/lapack/base/zgesv
*
*
* @example
* var zgesv = require( '@stdlib/lapack/base/zgesv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zgesv.ndarray( N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgesv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgesv = main;
} else {
	zgesv = tmp;
}


// EXPORTS //

module.exports = zgesv;

// exports: { "ndarray": "zgesv.ndarray" }
