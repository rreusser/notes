
'use strict';

/**
* Solves a general real tridiagonal system of linear equations.
*
* @module @stdlib/lapack/base/dgtsv
*
*
* @example
* var dgtsv = require( '@stdlib/lapack/base/dgtsv' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N, -10, 10, opts );
*
* dgtsv.ndarray( N, N, DL, 1, 0, d, 1, 0, DU, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgtsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtsv = main;
} else {
	dgtsv = tmp;
}


// EXPORTS //

module.exports = dgtsv;

// exports: { "ndarray": "dgtsv.ndarray" }
