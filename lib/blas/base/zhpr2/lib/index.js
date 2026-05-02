
'use strict';

/**
* Perform the Hermitian packed rank-2 update A := alpha_x_y**H + conj(alpha)_y_x**H + A.
*
* @module @stdlib/blas/base/zhpr2
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zhpr2 = require( '@stdlib/blas/base/zhpr2' );
*
* var x = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
* var y = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
* var AP = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
*
* zhpr2.ndarray( 'upper', 2, alpha, x, 1, 0, y, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpr2 = main;
} else {
	zhpr2 = tmp;
}


// EXPORTS //

module.exports = zhpr2;

// exports: { "ndarray": "zhpr2.ndarray" }
