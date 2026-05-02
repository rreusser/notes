

'use strict';

/**
* Reorder Schur factorization of a complex matrix
*
* @module @stdlib/lapack/base/ztrexc
*
*
* @example
* var ztrexc = require( '@stdlib/lapack/base/ztrexc' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
*
* ztrexc.ndarray( 'update', N, T, N, 1, 0, Q, N, 1, 0, 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrexc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrexc = main;
} else {
	ztrexc = tmp;
}


// EXPORTS //

module.exports = ztrexc;

// exports: { "ndarray": "ztrexc.ndarray" }
