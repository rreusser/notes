
'use strict';

/**
* Perform one of the matrix-vector operations x := A_x, x := A__T_x, or x := A*_H_x, where A is a triangular band matrix.
*
* @module @stdlib/blas/base/ztbmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var ztbmv = require( '@stdlib/blas/base/ztbmv' );
*
* var A = new Float64Array( [ 0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
* var x = new Float64Array( [ 1.0, 0.0, 1.0, 0.0 ] );
*
* ztbmv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, 1, A, 2, 1, 0, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztbmv = main;
} else {
	ztbmv = tmp;
}


// EXPORTS //

module.exports = ztbmv;

// exports: { "ndarray": "ztbmv.ndarray" }
