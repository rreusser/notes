

'use strict';

/**
* Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zlagtm
*
*
* @example
* var zlagtm = require( '@stdlib/lapack/base/zlagtm' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N, -10, 10, opts );
*
* zlagtm.ndarray( 'no-transpose', N, N, 1.0, DL, 1, 0, d, 1, 0, DU, 1, 0, X, N, 1, 0, 1.0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlagtm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlagtm = main;
} else {
	zlagtm = tmp;
}


// EXPORTS //

module.exports = zlagtm;

// exports: { "ndarray": "zlagtm.ndarray" }
