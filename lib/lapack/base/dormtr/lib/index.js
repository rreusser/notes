
'use strict';

/**
* Apply orthogonal matrix Q from dsytrd to a general matrix.
*
* @module @stdlib/lapack/base/dormtr
*
*
* @example
* var dormtr = require( '@stdlib/lapack/base/dormtr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dormtr.ndarray( 'left', 'upper', 'no-transpose', N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dormtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormtr = main;
} else {
	dormtr = tmp;
}


// EXPORTS //

module.exports = dormtr;

// exports: { "ndarray": "dormtr.ndarray" }
