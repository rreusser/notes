
'use strict';

/**
* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (RFP).
*
* @module @stdlib/lapack/base/dtrttf
*
*
* @example
* var dtrttf = require( '@stdlib/lapack/base/dtrttf' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var ARF = discreteUniform( N, -10, 10, opts );
*
* dtrttf.ndarray( 'no-transpose', 'upper', N, A, N, 1, 0, 1, ARF, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrttf = main;
} else {
	dtrttf = tmp;
}


// EXPORTS //

module.exports = dtrttf;

// exports: { "ndarray": "dtrttf.ndarray" }
