
'use strict';

/**
* Solves a system of linear equations with an LU factored matrix using complete pivoting.
*
* @module @stdlib/lapack/base/dgesc2
*
*
* @example
* var dgesc2 = require( '@stdlib/lapack/base/dgesc2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var RHS = discreteUniform( N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var JPIV = discreteUniform( N, -10, 10, opts );
*
* dgesc2.ndarray( N, A, N, 1, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, 1.0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgesc2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgesc2 = main;
} else {
	dgesc2 = tmp;
}


// EXPORTS //

module.exports = dgesc2;

// exports: { "ndarray": "dgesc2.ndarray" }
