
'use strict';

/**
* Provides error bounds for solution of a triangular system.
*
* @module @stdlib/lapack/base/dtrrfs
*
*
* @example
* var dtrrfs = require( '@stdlib/lapack/base/dtrrfs' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dtrrfs.ndarray( 'upper', 'no-transpose', 'non-unit', N, N, A, N, 1, 0, B, N, 1, 0, X, N, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrrfs = main;
} else {
	dtrrfs = tmp;
}


// EXPORTS //

module.exports = dtrrfs;

// exports: { "ndarray": "dtrrfs.ndarray" }
