

'use strict';

/**
* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (TF)
*
* @module @stdlib/lapack/base/ztrttf
*
*
* @example
* var ztrttf = require( '@stdlib/lapack/base/ztrttf' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var ARF = discreteUniform( N, -10, 10, opts );
*
* ztrttf.ndarray( 'no-transpose', 'upper', N, A, N, 1, 0, 1, ARF, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrttf = main;
} else {
	ztrttf = tmp;
}


// EXPORTS //

module.exports = ztrttf;

// exports: { "ndarray": "ztrttf.ndarray" }
