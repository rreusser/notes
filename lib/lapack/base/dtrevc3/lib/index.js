
'use strict';

/**
* Computes eigenvectors of a real upper quasi-triangular matrix.
*
* @module @stdlib/lapack/base/dtrevc3
*
*
* @example
* var dtrevc3 = require( '@stdlib/lapack/base/dtrevc3' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var VL = discreteUniform( N * N, -10, 10, opts );
* var VR = discreteUniform( N * N, -10, 10, opts );
* var SELECT = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dtrevc3.ndarray( 'left', 'all', SELECT, 1, 0, N, T, N, 1, 0, VL, N, 1, 0, VR, N, 1, 0, N, N, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrevc3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrevc3 = main;
} else {
	dtrevc3 = tmp;
}


// EXPORTS //

module.exports = dtrevc3;

// exports: { "ndarray": "dtrevc3.ndarray" }
