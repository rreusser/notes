

'use strict';

/**
* Copies a complex triangular matrix from packed storage to full storage
*
* @module @stdlib/lapack/base/ztpttr
*
*
* @example
* var ztpttr = require( '@stdlib/lapack/base/ztpttr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AP = discreteUniform( N, -10, 10, opts );
*
* ztpttr.ndarray( 'upper', N, AP, 1, 0, A, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztpttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpttr = main;
} else {
	ztpttr = tmp;
}


// EXPORTS //

module.exports = ztpttr;

// exports: { "ndarray": "ztpttr.ndarray" }
