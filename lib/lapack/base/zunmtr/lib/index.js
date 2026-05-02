

'use strict';

/**
* Applies a complex unitary matrix from zhetrd to a matrix
*
* @module @stdlib/lapack/base/zunmtr
*
*
* @example
* var zunmtr = require( '@stdlib/lapack/base/zunmtr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zunmtr.ndarray( 'left', 'upper', 'no-transpose', N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zunmtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmtr = main;
} else {
	zunmtr = tmp;
}


// EXPORTS //

module.exports = zunmtr;

// exports: { "ndarray": "zunmtr.ndarray" }
