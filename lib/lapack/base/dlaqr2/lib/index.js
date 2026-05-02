
'use strict';

/**
* Performs aggressive early deflation on an upper Hessenberg matrix.
*
* @module @stdlib/lapack/base/dlaqr2
*
*
* @example
* var dlaqr2 = require( '@stdlib/lapack/base/dlaqr2' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var V = discreteUniform( N * N, -10, 10, opts );
* var T = discreteUniform( N * N, -10, 10, opts );
* var WV = discreteUniform( N * N, -10, 10, opts );
* var SR = discreteUniform( N, -10, 10, opts );
* var SI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlaqr2.ndarray( 1, 1, N, 1, 1, 1, H, N, 1, 0, 1, 1, Z, N, 1, 0, 1, 1, SR, 1, 0, SI, 1, 0, V, N, 1, 0, 1, T, N, 1, 0, 1, WV, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr2 = main;
} else {
	dlaqr2 = tmp;
}


// EXPORTS //

module.exports = dlaqr2;

// exports: { "ndarray": "dlaqr2.ndarray" }
