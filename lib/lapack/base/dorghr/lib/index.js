
'use strict';

/**
* Generates the orthogonal matrix Q from Hessenberg reduction.
*
* @module @stdlib/lapack/base/dorghr
*
*
* @example
* var dorghr = require( '@stdlib/lapack/base/dorghr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dorghr.ndarray( N, N, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dorghr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dorghr = main;
} else {
	dorghr = tmp;
}


// EXPORTS //

module.exports = dorghr;

// exports: { "ndarray": "dorghr.ndarray" }
