
'use strict';

/**
* Computes the norm of a real triangular matrix.
*
* @module @stdlib/lapack/base/dlantr
*
*
* @example
* var dlantr = require( '@stdlib/lapack/base/dlantr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlantr.ndarray( '1', 'upper', 'non-unit', N, N, A, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlantr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlantr = main;
} else {
	dlantr = tmp;
}


// EXPORTS //

module.exports = dlantr;

// exports: { "ndarray": "dlantr.ndarray" }
