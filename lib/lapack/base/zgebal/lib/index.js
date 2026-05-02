
'use strict';

/**
* Balances a general complex matrix for eigenvalue computation.
*
* @module @stdlib/lapack/base/zgebal
*
*
* @example
* var zgebal = require( '@stdlib/lapack/base/zgebal' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var SCALE = discreteUniform( N, -10, 10, opts );
*
* zgebal.ndarray( 'both', N, A, N, 1, 0, N, N, SCALE, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgebal;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgebal = main;
} else {
	zgebal = tmp;
}


// EXPORTS //

module.exports = zgebal;

// exports: { "ndarray": "zgebal.ndarray" }
