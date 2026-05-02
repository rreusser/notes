

'use strict';

/**
* Compute the solution to a complex system of linear equations A * X = B where A is Hermitian positive definite
*
* @module @stdlib/lapack/base/zposv
*
*
* @example
* var zposv = require( '@stdlib/lapack/base/zposv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
*
* zposv.ndarray( 'upper', N, N, A, N, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zposv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zposv = main;
} else {
	zposv = tmp;
}


// EXPORTS //

module.exports = zposv;

// exports: { "ndarray": "zposv.ndarray" }
