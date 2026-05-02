
'use strict';

/**
* Solve a triangular packed system of equations.
*
* @module @stdlib/blas/base/dtpsv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtpsv = require( '@stdlib/blas/base/dtpsv' );
*
* var AP = new Float64Array( [ 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 5.0, 11.0 ] );
*
* dtpsv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, AP, 1, 0, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtpsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpsv = main;
} else {
	dtpsv = tmp;
}


// EXPORTS //

module.exports = dtpsv;

// exports: { "ndarray": "dtpsv.ndarray" }
