

'use strict';

/**
* Improves solution to a complex Hermitian tridiagonal system and provides error bounds
*
* @module @stdlib/lapack/base/zptrfs
*
*
* @example
* var zptrfs = require( '@stdlib/lapack/base/zptrfs' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var DF = discreteUniform( N, -10, 10, opts );
* var EF = discreteUniform( N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zptrfs.ndarray( 'upper', N, N, d, 1, 0, e, 1, 0, DF, 1, 0, EF, 1, 0, B, N, 1, 0, X, N, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zptrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zptrfs = main;
} else {
	zptrfs = tmp;
}


// EXPORTS //

module.exports = zptrfs;

// exports: { "ndarray": "zptrfs.ndarray" }
