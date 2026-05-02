
'use strict';

/**
* Perform the Hermitian banded matrix-vector operation y := alpha_A_x + beta*y.
*
* @module @stdlib/blas/base/zhbmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zhbmv = require( '@stdlib/blas/base/zhbmv' );
*
* var A = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
* var x = new Float64Array( [ 1.0, 0.0, 1.0, 0.0 ] );
* var y = new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
* var beta = new Float64Array( [ 0.0, 0.0 ] );
*
* zhbmv.ndarray( 'upper', 2, 1, alpha, A, 2, 1, 0, x, 1, 0, beta, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhbmv = main;
} else {
	zhbmv = tmp;
}


// EXPORTS //

module.exports = zhbmv;

// exports: { "ndarray": "zhbmv.ndarray" }
