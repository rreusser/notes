
'use strict';

/**
* Equilibrate a symmetric matrix using scaling factors.
*
* @module @stdlib/lapack/base/dlaqsy
*
*
* @example
* var dlaqsy = require( '@stdlib/lapack/base/dlaqsy' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* dlaqsy.ndarray( 'upper', N, A, N, 1, 0, s, 1, 0, 1, 1, 'none' );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqsy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqsy = main;
} else {
	dlaqsy = tmp;
}


// EXPORTS //

module.exports = dlaqsy;

// exports: { "ndarray": "dlaqsy.ndarray" }
