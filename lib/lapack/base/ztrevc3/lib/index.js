

'use strict';

/**
* Computes eigenvectors of a complex upper triangular matrix
*
* @module @stdlib/lapack/base/ztrevc3
*
*
* @example
* var ztrevc3 = require( '@stdlib/lapack/base/ztrevc3' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var VL = discreteUniform( N * N, -10, 10, opts );
* var VR = discreteUniform( N * N, -10, 10, opts );
* var SELECT = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* ztrevc3.ndarray( 'left', 'all', SELECT, 1, 0, N, T, N, 1, 0, VL, N, 1, 0, VR, N, 1, 0, N, N, WORK, 1, 0, N, RWORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrevc3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrevc3 = main;
} else {
	ztrevc3 = tmp;
}


// EXPORTS //

module.exports = ztrevc3;

// exports: { "ndarray": "ztrevc3.ndarray" }
