
'use strict';

/**
* Performs aggressive early deflation with blocked operations.
*
* @module @stdlib/lapack/base/dlaqr3
*
*
* @example
* var dlaqr3 = require( '@stdlib/lapack/base/dlaqr3' );
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
* dlaqr3.ndarray( 1, 1, N, 1, 1, 1, H, N, 1, 0, 1, 1, Z, N, 1, 0, 1, 1, SR, 1, 0, SI, 1, 0, V, N, 1, 0, 1, T, N, 1, 0, 1, WV, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqr3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr3 = main;
} else {
	dlaqr3 = tmp;
}


// EXPORTS //

module.exports = dlaqr3;

// exports: { "ndarray": "dlaqr3.ndarray" }
