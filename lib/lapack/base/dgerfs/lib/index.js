
'use strict';

/**
* Improves the solution to A*X = B using iterative refinement.
*
* @module @stdlib/lapack/base/dgerfs
*
*
* @example
* var dgerfs = require( '@stdlib/lapack/base/dgerfs' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
*
* dgerfs.ndarray( 'no-transpose', N, N, A, N, 1, 0, AF, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, X, N, 1, 0, FERR, 1, 0, BERR, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgerfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgerfs = main;
} else {
	dgerfs = tmp;
}


// EXPORTS //

module.exports = dgerfs;

// exports: { "ndarray": "dgerfs.ndarray" }
