
'use strict';

/**
* Solve a complex general tridiagonal system of linear equations A * X = B.
*
* @module @stdlib/lapack/base/zgtsv
*
*
* @example
* var zgtsv = require( '@stdlib/lapack/base/zgtsv' );
*
* var N = 3;
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N, -10, 10, opts );
*
* zgtsv.ndarray( N, N, DL, 1, 0, d, 1, 0, DU, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgtsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtsv = main;
} else {
	zgtsv = tmp;
}


// EXPORTS //

module.exports = zgtsv;

// exports: { "ndarray": "zgtsv.ndarray" }
