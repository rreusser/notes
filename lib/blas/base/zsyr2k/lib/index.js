
'use strict';

/**
* Perform one of the symmetric rank-2k operations C := alpha_A_B**T + alpha_B_A**T + beta_C or C := alpha_A**T_B + alpha_B**T_A + beta_C.
*
* @module @stdlib/blas/base/zsyr2k
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zsyr2k = require( '@stdlib/blas/base/zsyr2k' );
*
* var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var B = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var C = new Float64Array( 8 );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
* var beta = new Float64Array( [ 0.0, 0.0 ] );
*
* zsyr2k.ndarray( 'upper', 'no-transpose', 2, 2, alpha, A, 2, 1, 0, B, 2, 1, 0, beta, C, 2, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsyr2k;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsyr2k = main;
} else {
	zsyr2k = tmp;
}


// EXPORTS //

module.exports = zsyr2k;

// exports: { "ndarray": "zsyr2k.ndarray" }
