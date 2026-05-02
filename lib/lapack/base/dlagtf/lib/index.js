
'use strict';

/**
* Factorizes the matrix (T - lambda*I) where T is a tridiagonal matrix.
*
* @module @stdlib/lapack/base/dlagtf
*
*
* @example
* var dlagtf = require( '@stdlib/lapack/base/dlagtf' );
*
* var N = 3;
* var a = discreteUniform( N, -10, 10, opts );
* var b = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var IN = discreteUniform( N, -10, 10, opts );
*
* dlagtf.ndarray( N, a, 1, 0, 1, b, 1, 0, c, 1, 0, 1.0, d, 1, 0, IN, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlagtf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlagtf = main;
} else {
	dlagtf = tmp;
}


// EXPORTS //

module.exports = dlagtf;

// exports: { "ndarray": "dlagtf.ndarray" }
