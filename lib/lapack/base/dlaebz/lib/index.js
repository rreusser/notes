
'use strict';

/**
* Auxiliary bisection routine for tridiagonal eigenvalue computation.
*
* @module @stdlib/lapack/base/dlaebz
*
*
* @example
* var dlaebz = require( '@stdlib/lapack/base/dlaebz' );
*
* var N = 3;
* var e = discreteUniform( N * N, -10, 10, opts );
* var AB = discreteUniform( N * N, -10, 10, opts );
* var NAB = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N * N, -10, 10, opts );
* var NVAL = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dlaebz.ndarray( 1, 1, N, 1, 1, 1, 1.0, 1, 1.0, d, 1, 0, e, 1, 0, 1, 1, 0, NVAL, 1, 0, AB, N, 1, 0, c, 1, 0, 1, NAB, N, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaebz;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaebz = main;
} else {
	dlaebz = tmp;
}


// EXPORTS //

module.exports = dlaebz;

// exports: { "ndarray": "dlaebz.ndarray" }
