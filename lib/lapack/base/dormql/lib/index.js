
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from QL factorization (blocked).
*
* @module @stdlib/lapack/base/dormql
*
*
* @example
* var dormql = require( '@stdlib/lapack/base/dormql' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dormql.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dormql;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormql = main;
} else {
	dormql = tmp;
}


// EXPORTS //

module.exports = dormql;

// exports: { "ndarray": "dormql.ndarray" }
