

'use strict';

/**
* Computes eigenvalues and Schur form using multishift QR with aggressive early deflation
*
* @module @stdlib/lapack/base/dlaqr0
*
*
* @example
* var dlaqr0 = require( '@stdlib/lapack/base/dlaqr0' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlaqr0.ndarray( 1, 1, N, N, N, H, N, 1, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqr0;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr0 = main;
} else {
	dlaqr0 = tmp;
}


// EXPORTS //

module.exports = dlaqr0;

// exports: { "ndarray": "dlaqr0.ndarray" }
