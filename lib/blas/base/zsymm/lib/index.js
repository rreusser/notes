
'use strict';

/**
* Perform one of the symmetric matrix-matrix operations C := alpha_A_B + beta_C or C := alpha_B_A + beta_C.
*
* @module @stdlib/blas/base/zsymm
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zsymm = require( '@stdlib/blas/base/zsymm' );
*
* var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var B = new Float64Array( [ 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0 ] );
* var C = new Float64Array( 8 );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
* var beta = new Float64Array( [ 0.0, 0.0 ] );
*
* zsymm.ndarray( 'left', 'upper', 2, 2, alpha, A, 2, 1, 0, B, 2, 1, 0, beta, C, 2, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsymm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsymm = main;
} else {
	zsymm = tmp;
}


// EXPORTS //

module.exports = zsymm;

// exports: { "ndarray": "zsymm.ndarray" }
