

'use strict';

/**
* Iterative refinement for Hermitian positive definite system
*
* @module @stdlib/lapack/base/zporfs
*
*
* @example
* var zporfs = require( '@stdlib/lapack/base/zporfs' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zporfs.ndarray( 'upper', N, N, A, N, 1, 0, AF, N, 1, 0, B, N, 1, 0, X, N, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zporfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zporfs = main;
} else {
	zporfs = tmp;
}


// EXPORTS //

module.exports = zporfs;

// exports: { "ndarray": "zporfs.ndarray" }
