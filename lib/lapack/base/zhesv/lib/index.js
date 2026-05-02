

'use strict';

/**
* Complex Hermitian indefinite linear system solver
*
* @module @stdlib/lapack/base/zhesv
*
*
* @example
* var zhesv = require( '@stdlib/lapack/base/zhesv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zhesv.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhesv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhesv = main;
} else {
	zhesv = tmp;
}


// EXPORTS //

module.exports = zhesv;

// exports: { "ndarray": "zhesv.ndarray" }
