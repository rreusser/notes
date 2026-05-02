

'use strict';

/**
* Copy a triangular matrix from Rectangular Full Packed format (RFP) to standard full format (TR)
*
* @module @stdlib/lapack/base/ztfttr
*
*
* @example
* var ztfttr = require( '@stdlib/lapack/base/ztfttr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var ARF = discreteUniform( N, -10, 10, opts );
*
* ztfttr.ndarray( 'no-transpose', 'upper', N, ARF, 1, 0, A, N, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztfttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztfttr = main;
} else {
	ztfttr = tmp;
}


// EXPORTS //

module.exports = ztfttr;

// exports: { "ndarray": "ztfttr.ndarray" }
