
'use strict';

/**
* Reorders the real Schur factorization by an orthogonal similarity transformation.
*
* @module @stdlib/lapack/base/dtrexc
*
*
* @example
* var dtrexc = require( '@stdlib/lapack/base/dtrexc' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dtrexc.ndarray( 'update', N, T, N, 1, 0, Q, N, 1, 0, 1, 1, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrexc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrexc = main;
} else {
	dtrexc = tmp;
}


// EXPORTS //

module.exports = dtrexc;

// exports: { "ndarray": "dtrexc.ndarray" }
