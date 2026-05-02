
'use strict';

/**
* Perform one of the triangular packed matrix-vector operations x := A_x or x := A__T_x or x := A*_H_x.
*
* @module @stdlib/blas/base/ztpmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var ztpmv = require( '@stdlib/blas/base/ztpmv' );
*
* var AP = new Float64Array( [ 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
* var x = new Float64Array( [ 1.0, 0.0, 1.0, 0.0 ] );
*
* ztpmv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, AP, 1, 0, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztpmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpmv = main;
} else {
	ztpmv = tmp;
}


// EXPORTS //

module.exports = ztpmv;

// exports: { "ndarray": "ztpmv.ndarray" }
