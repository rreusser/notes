
'use strict';

/**
* Permute columns of a matrix.
*
* @module @stdlib/lapack/base/dlapmt
*
*
* @example
* var dlapmt = require( '@stdlib/lapack/base/dlapmt' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var k = discreteUniform( N, -10, 10, opts );
*
* dlapmt.ndarray( 1, N, N, X, N, 1, 0, k, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlapmt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlapmt = main;
} else {
	dlapmt = tmp;
}


// EXPORTS //

module.exports = dlapmt;

// exports: { "ndarray": "dlapmt.ndarray" }
