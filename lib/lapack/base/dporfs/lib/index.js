
'use strict';

/**
* Improves solution to a symmetric positive definite system and provides error bounds.
*
* @module @stdlib/lapack/base/dporfs
*
*
* @example
* var dporfs = require( '@stdlib/lapack/base/dporfs' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dporfs.ndarray( 'upper', N, N, A, N, 1, 0, AF, N, 1, 0, B, N, 1, 0, X, N, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dporfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dporfs = main;
} else {
	dporfs = tmp;
}


// EXPORTS //

module.exports = dporfs;

// exports: { "ndarray": "dporfs.ndarray" }
