
'use strict';

/**
* Computes the LDL^T factorization of a real symmetric positive definite tridiagonal matrix.
*
* @module @stdlib/lapack/base/dpttrf
*
*
* @example
* var dpttrf = require( '@stdlib/lapack/base/dpttrf' );
*
* var N = 3;
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
*
* dpttrf.ndarray( N, d, 1, 0, e, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpttrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpttrf = main;
} else {
	dpttrf = tmp;
}


// EXPORTS //

module.exports = dpttrf;

// exports: { "ndarray": "dpttrf.ndarray" }
