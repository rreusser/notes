

'use strict';

/**
* Computes the solution to a complex Hermitian positive definite banded system of linear equations A * X = B
*
* @module @stdlib/lapack/base/zpbsv
*
*
* @example
* var zpbsv = require( '@stdlib/lapack/base/zpbsv' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
*
* zpbsv.ndarray( 'upper', N, 1, N, AB, N, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpbsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpbsv = main;
} else {
	zpbsv = tmp;
}


// EXPORTS //

module.exports = zpbsv;

// exports: { "ndarray": "zpbsv.ndarray" }
