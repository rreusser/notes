
'use strict';

/**
* Swaps adjacent diagonal blocks of a real upper quasi-triangular matrix.
*
* @module @stdlib/lapack/base/dlaexc
*
*
* @example
* var dlaexc = require( '@stdlib/lapack/base/dlaexc' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlaexc.ndarray( 1, N, T, N, 1, 0, Q, N, 1, 0, 1, 1, 1, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaexc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaexc = main;
} else {
	dlaexc = tmp;
}


// EXPORTS //

module.exports = dlaexc;

// exports: { "ndarray": "dlaexc.ndarray" }
