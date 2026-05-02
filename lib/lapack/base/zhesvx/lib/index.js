

'use strict';

/**
* Complex Hermitian indefinite expert solver
*
* @module @stdlib/lapack/base/zhesvx
*
*
* @example
* var zhesvx = require( '@stdlib/lapack/base/zhesvx' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zhesvx.ndarray( 'not-factored', 'upper', N, N, A, N, 1, 0, AF, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, X, N, 1, 0, 1.0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, N, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhesvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhesvx = main;
} else {
	zhesvx = tmp;
}


// EXPORTS //

module.exports = zhesvx;

// exports: { "ndarray": "zhesvx.ndarray" }
