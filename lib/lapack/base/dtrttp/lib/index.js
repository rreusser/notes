
'use strict';

/**
* Copy a triangular matrix from full format to standard packed format.
*
* @module @stdlib/lapack/base/dtrttp
*
*
* @example
* var dtrttp = require( '@stdlib/lapack/base/dtrttp' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AP = discreteUniform( N, -10, 10, opts );
*
* dtrttp.ndarray( 'upper', N, A, N, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrttp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrttp = main;
} else {
	dtrttp = tmp;
}


// EXPORTS //

module.exports = dtrttp;

// exports: { "ndarray": "dtrttp.ndarray" }
