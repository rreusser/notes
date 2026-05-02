

'use strict';

/**
* Copy a complex triangular matrix from full format (TR) to standard packed format (TP)
*
* @module @stdlib/lapack/base/ztrttp
*
*
* @example
* var ztrttp = require( '@stdlib/lapack/base/ztrttp' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AP = discreteUniform( N, -10, 10, opts );
*
* ztrttp.ndarray( 'upper', N, A, N, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrttp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrttp = main;
} else {
	ztrttp = tmp;
}


// EXPORTS //

module.exports = ztrttp;

// exports: { "ndarray": "ztrttp.ndarray" }
