
'use strict';

/**
* Solves a real symmetric positive definite tridiagonal system of linear equations.
*
* @module @stdlib/lapack/base/dptsv
*
*
* @example
* var dptsv = require( '@stdlib/lapack/base/dptsv' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* dptsv.ndarray( N, N, d, 1, 0, e, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dptsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dptsv = main;
} else {
	dptsv = tmp;
}


// EXPORTS //

module.exports = dptsv;

// exports: { "ndarray": "dptsv.ndarray" }
