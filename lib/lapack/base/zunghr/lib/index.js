

'use strict';

/**
* Generates the unitary matrix Q from Hessenberg reduction
*
* @module @stdlib/lapack/base/zunghr
*
*
* @example
* var zunghr = require( '@stdlib/lapack/base/zunghr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zunghr.ndarray( N, N, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zunghr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunghr = main;
} else {
	zunghr = tmp;
}


// EXPORTS //

module.exports = zunghr;

// exports: { "ndarray": "zunghr.ndarray" }
