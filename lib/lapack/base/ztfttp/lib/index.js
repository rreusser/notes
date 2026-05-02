

'use strict';

/**
* Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).
*
* @module @stdlib/lapack/base/ztfttp
*
*
* @example
* var ztfttp = require( '@stdlib/lapack/base/ztfttp' );
*
* var N = 3;
* var ARF = discreteUniform( N, -10, 10, opts );
* var AP = discreteUniform( N, -10, 10, opts );
*
* ztfttp.ndarray( 'no-transpose', 'upper', N, ARF, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztfttp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztfttp = main;
} else {
	ztfttp = tmp;
}


// EXPORTS //

module.exports = ztfttp;

// exports: { "ndarray": "ztfttp.ndarray" }
