
'use strict';

/**
* Compute row and column scalings for a complex general matrix.
*
* @module @stdlib/lapack/base/zgeequ
*
*
* @example
* var zgeequ = require( '@stdlib/lapack/base/zgeequ' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var r = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
*
* zgeequ.ndarray( N, N, A, N, 1, 0, r, 1, 0, c, 1, 0, 1, 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgeequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgeequ = main;
} else {
	zgeequ = tmp;
}


// EXPORTS //

module.exports = zgeequ;

// exports: { "ndarray": "zgeequ.ndarray" }
