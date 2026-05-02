
'use strict';

/**
* Compute the RQ factorization of a real matrix (blocked).
*
* @module @stdlib/lapack/base/dgerqf
*
*
* @example
* var dgerqf = require( '@stdlib/lapack/base/dgerqf' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dgerqf.ndarray( N, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgerqf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgerqf = main;
} else {
	dgerqf = tmp;
}


// EXPORTS //

module.exports = dgerqf;

// exports: { "ndarray": "dgerqf.ndarray" }
