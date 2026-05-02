
'use strict';

/**
* Perform one of the matrix-vector operations y := alpha_A_x + beta_y or y := alpha_A**T_x + beta_y or y := alpha*A**H_x + beta_y where A is a general band matrix.
*
* @module @stdlib/blas/base/zgbmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zgbmv = require( '@stdlib/blas/base/zgbmv' );
*
* var A = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
* var x = new Float64Array( [ 1.0, 0.0, 1.0, 0.0 ] );
* var y = new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
* var beta = new Float64Array( [ 0.0, 0.0 ] );
*
* zgbmv.ndarray( 'no-transpose', 2, 2, 0, 1, alpha, A, 2, 1, 0, x, 1, 0, beta, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbmv = main;
} else {
	zgbmv = tmp;
}


// EXPORTS //

module.exports = zgbmv;

// exports: { "ndarray": "zgbmv.ndarray" }
