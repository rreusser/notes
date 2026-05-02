
'use strict';

/**
* Solve one of the triangular packed systems A_x = b or A__T_x = b or A*_H_x = b.
*
* @module @stdlib/blas/base/ztpsv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var ztpsv = require( '@stdlib/blas/base/ztpsv' );
*
* var AP = new Float64Array( [ 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
* var x = new Float64Array( [ 5.0, 0.0, 11.0, 0.0 ] );
*
* ztpsv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, AP, 1, 0, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztpsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpsv = main;
} else {
	ztpsv = tmp;
}


// EXPORTS //

module.exports = ztpsv;

// exports: { "ndarray": "ztpsv.ndarray" }
