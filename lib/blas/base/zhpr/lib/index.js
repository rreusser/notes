
'use strict';

/**
* Perform the Hermitian packed rank-1 update A := alpha_x_x**H + A.
*
* @module @stdlib/blas/base/zhpr
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zhpr = require( '@stdlib/blas/base/zhpr' );
*
* var x = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
* var AP = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
*
* zhpr.ndarray( 'upper', 2, 1.0, x, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpr = main;
} else {
	zhpr = tmp;
}


// EXPORTS //

module.exports = zhpr;

// exports: { "ndarray": "zhpr.ndarray" }
