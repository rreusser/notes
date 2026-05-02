
'use strict';

/**
* Equilibrates a general matrix using row and column scaling factors.
*
* @module @stdlib/lapack/base/dlaqge
*
*
* @example
* var dlaqge = require( '@stdlib/lapack/base/dlaqge' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var r = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
*
* dlaqge.ndarray( N, N, A, N, 1, 0, r, 1, 0, c, 1, 0, 1, 1, 1, 'none' );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqge;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqge = main;
} else {
	dlaqge = tmp;
}


// EXPORTS //

module.exports = dlaqge;

// exports: { "ndarray": "dlaqge.ndarray" }
