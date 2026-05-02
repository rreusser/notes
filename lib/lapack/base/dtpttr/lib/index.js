
'use strict';

/**
* Copy a triangular matrix from standard packed format to full format.
*
* @module @stdlib/lapack/base/dtpttr
*
*
* @example
* var dtpttr = require( '@stdlib/lapack/base/dtpttr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AP = discreteUniform( N, -10, 10, opts );
*
* dtpttr.ndarray( 'upper', N, AP, 1, 0, A, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtpttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpttr = main;
} else {
	dtpttr = tmp;
}


// EXPORTS //

module.exports = dtpttr;

// exports: { "ndarray": "dtpttr.ndarray" }
