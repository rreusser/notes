
'use strict';

/**
* Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).
*
* @module @stdlib/lapack/base/dtpttf
*
*
* @example
* var dtpttf = require( '@stdlib/lapack/base/dtpttf' );
*
* var N = 3;
* var AP = discreteUniform( N, -10, 10, opts );
* var ARF = discreteUniform( N, -10, 10, opts );
*
* dtpttf.ndarray( 'no-transpose', 'upper', N, AP, 1, 0, ARF, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtpttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpttf = main;
} else {
	dtpttf = tmp;
}


// EXPORTS //

module.exports = dtpttf;

// exports: { "ndarray": "dtpttf.ndarray" }
