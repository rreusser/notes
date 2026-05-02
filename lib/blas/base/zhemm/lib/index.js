
'use strict';

/**
* Performs Hermitian matrix-matrix multiplication.
*
* @module @stdlib/blas/base/zhemm
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zhemm = require( '@stdlib/blas/base/zhemm' );
*
* var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var B = new Float64Array( [ 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0 ] );
* var C = new Float64Array( 8 );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
* var beta = new Float64Array( [ 0.0, 0.0 ] );
*
* zhemm.ndarray( 'left', 'upper', 2, 2, alpha, A, 2, 1, 0, B, 2, 1, 0, beta, C, 2, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhemm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhemm = main;
} else {
	zhemm = tmp;
}


// EXPORTS //

module.exports = zhemm;

// exports: { "ndarray": "zhemm.ndarray" }
