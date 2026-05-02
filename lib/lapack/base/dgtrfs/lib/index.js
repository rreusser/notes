
'use strict';

/**
* Iterative refinement for a general tridiagonal system.
*
* @module @stdlib/lapack/base/dgtrfs
*
*
* @example
* var dgtrfs = require( '@stdlib/lapack/base/dgtrfs' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var DLF = discreteUniform( N, -10, 10, opts );
* var DF = discreteUniform( N, -10, 10, opts );
* var DUF = discreteUniform( N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dgtrfs.ndarray( 'no-transpose', N, N, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, 1, 1, 0, IPIV, 1, 0, B, N, 1, 0, X, N, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgtrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtrfs = main;
} else {
	dgtrfs = tmp;
}


// EXPORTS //

module.exports = dgtrfs;

// exports: { "ndarray": "dgtrfs.ndarray" }
