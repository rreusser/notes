

'use strict';

/**
* Computes row and column scalings for equilibrating a general matrix
*
* @module @stdlib/lapack/base/dgeequ
*
*
* @example
* var dgeequ = require( '@stdlib/lapack/base/dgeequ' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var r = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
*
* dgeequ.ndarray( N, N, A, N, 1, 0, r, 1, 0, c, 1, 0, 1, 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgeequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeequ = main;
} else {
	dgeequ = tmp;
}


// EXPORTS //

module.exports = dgeequ;

// exports: { "ndarray": "dgeequ.ndarray" }
