
'use strict';

/**
* Performs a single small-bulge multi-shift QR sweep.
*
* @module @stdlib/lapack/base/dlaqr5
*
*
* @example
* var dlaqr5 = require( '@stdlib/lapack/base/dlaqr5' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var V = discreteUniform( N * N, -10, 10, opts );
* var U = discreteUniform( N * N, -10, 10, opts );
* var WV = discreteUniform( N * N, -10, 10, opts );
* var WH = discreteUniform( N * N, -10, 10, opts );
* var SR = discreteUniform( N, -10, 10, opts );
* var SI = discreteUniform( N, -10, 10, opts );
*
* dlaqr5.ndarray( 1, 1, 1, N, 1, 1, 1, SR, 1, 0, SI, 1, 0, H, N, 1, 0, 1, 1, Z, N, 1, 0, V, N, 1, 0, U, N, 1, 0, 1, WV, N, 1, 0, 1, WH, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqr5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr5 = main;
} else {
	dlaqr5 = tmp;
}


// EXPORTS //

module.exports = dlaqr5;

// exports: { "ndarray": "dlaqr5.ndarray" }
