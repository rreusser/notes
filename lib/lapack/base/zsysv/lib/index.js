

'use strict';

/**
* Solves a complex symmetric indefinite system using Bunch-Kaufman factorization
*
* @module @stdlib/lapack/base/zsysv
*
*
* @example
* var zsysv = require( '@stdlib/lapack/base/zsysv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zsysv.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsysv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsysv = main;
} else {
	zsysv = tmp;
}


// EXPORTS //

module.exports = zsysv;

// exports: { "ndarray": "zsysv.ndarray" }
