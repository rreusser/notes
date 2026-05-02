
'use strict';

/**
* Multiply a general tridiagonal matrix by a rectangular matrix.
*
* @module @stdlib/lapack/base/dlagtm
*
*
* @example
* var dlagtm = require( '@stdlib/lapack/base/dlagtm' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N, -10, 10, opts );
*
* dlagtm.ndarray( 'no-transpose', N, N, 1.0, DL, 1, 0, d, 1, 0, DU, 1, 0, X, N, 1, 0, 1.0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlagtm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlagtm = main;
} else {
	dlagtm = tmp;
}


// EXPORTS //

module.exports = dlagtm;

// exports: { "ndarray": "dlagtm.ndarray" }
