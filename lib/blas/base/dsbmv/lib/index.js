
'use strict';

/**
* Perform matrix-vector operation with a symmetric band matrix.
*
* @module @stdlib/blas/base/dsbmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbmv = require( '@stdlib/blas/base/dsbmv' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 1.0 ] );
* var y = new Float64Array( [ 0.0, 0.0 ] );
*
* dsbmv.ndarray( 'upper', 2, 1, 1.0, A, 2, 1, 0, x, 1, 0, 0.0, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsbmv = main;
} else {
	dsbmv = tmp;
}


// EXPORTS //

module.exports = dsbmv;

// exports: { "ndarray": "dsbmv.ndarray" }
