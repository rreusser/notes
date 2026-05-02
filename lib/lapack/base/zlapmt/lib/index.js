

'use strict';

/**
* Rearrange columns of a complex matrix as specified by a permutation vector
*
* @module @stdlib/lapack/base/zlapmt
*
*
* @example
* var zlapmt = require( '@stdlib/lapack/base/zlapmt' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var k = discreteUniform( N, -10, 10, opts );
*
* zlapmt.ndarray( 1, N, N, X, N, 1, 0, k, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlapmt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlapmt = main;
} else {
	zlapmt = tmp;
}


// EXPORTS //

module.exports = zlapmt;

// exports: { "ndarray": "zlapmt.ndarray" }
