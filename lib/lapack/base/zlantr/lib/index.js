

'use strict';

/**
* Computes the norm of a complex triangular matrix
*
* @module @stdlib/lapack/base/zlantr
*
*
* @example
* var zlantr = require( '@stdlib/lapack/base/zlantr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zlantr.ndarray( '1', 'upper', 'non-unit', N, N, A, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlantr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlantr = main;
} else {
	zlantr = tmp;
}


// EXPORTS //

module.exports = zlantr;

// exports: { "ndarray": "zlantr.ndarray" }
