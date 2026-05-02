
'use strict';

/**
* Solves a real symmetric positive definite tridiagonal system using LDL^T factorization.
*
* @module @stdlib/lapack/base/dpttrs
*
*
* @example
* var dpttrs = require( '@stdlib/lapack/base/dpttrs' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* dpttrs.ndarray( N, N, d, 1, 0, e, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpttrs = main;
} else {
	dpttrs = tmp;
}


// EXPORTS //

module.exports = dpttrs;

// exports: { "ndarray": "dpttrs.ndarray" }
