
'use strict';

/**
* Solves a tridiagonal system using the LDL^T factorization from dpttrf.
*
* @module @stdlib/lapack/base/dptts2
*
*
* @example
* var dptts2 = require( '@stdlib/lapack/base/dptts2' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* dptts2.ndarray( N, N, d, 1, 0, e, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dptts2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dptts2 = main;
} else {
	dptts2 = tmp;
}


// EXPORTS //

module.exports = dptts2;

// exports: { "ndarray": "dptts2.ndarray" }
