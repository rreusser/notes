
'use strict';

/**
* Copy a triangular matrix from Rectangular Full Packed format to standard full format.
*
* @module @stdlib/lapack/base/dtfttr
*
*
* @example
* var dtfttr = require( '@stdlib/lapack/base/dtfttr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var ARF = discreteUniform( N, -10, 10, opts );
*
* dtfttr.ndarray( 'no-transpose', 'upper', N, ARF, 1, 0, A, N, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtfttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtfttr = main;
} else {
	dtfttr = tmp;
}


// EXPORTS //

module.exports = dtfttr;

// exports: { "ndarray": "dtfttr.ndarray" }
