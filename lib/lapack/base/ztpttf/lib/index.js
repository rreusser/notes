

'use strict';

/**
* Copy a triangular matrix from standard packed format (TP) to rectangular full packed format (RFP), complex version.
*
* @module @stdlib/lapack/base/ztpttf
*
*
* @example
* var ztpttf = require( '@stdlib/lapack/base/ztpttf' );
*
* var N = 3;
* var AP = discreteUniform( N, -10, 10, opts );
* var ARF = discreteUniform( N, -10, 10, opts );
*
* ztpttf.ndarray( 'no-transpose', 'upper', N, AP, 1, 0, ARF, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztpttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpttf = main;
} else {
	ztpttf = tmp;
}


// EXPORTS //

module.exports = ztpttf;

// exports: { "ndarray": "ztpttf.ndarray" }
