
'use strict';

/**
* Applies a complex unitary matrix Q from a QL factorization to a matrix (blocked).
*
* @module @stdlib/lapack/base/zunmql
*
* @example
* var zunmql = require( '@stdlib/lapack/base/zunmql' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zunmql.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zunmql;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmql = main;
} else {
	zunmql = tmp;
}


// EXPORTS //

module.exports = zunmql;

// exports: { "ndarray": "zunmql.ndarray" }
