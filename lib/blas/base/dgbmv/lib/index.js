
'use strict';

/**
* Perform matrix-vector operation with a general band matrix.
*
* @module @stdlib/blas/base/dgbmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgbmv = require( '@stdlib/blas/base/dgbmv' );
*
* var A = new Float64Array( [ 0.0, 1.0, 2.0, 3.0, 4.0, 0.0 ] );
* var x = new Float64Array( [ 1.0, 1.0, 1.0 ] );
* var y = new Float64Array( [ 0.0, 0.0, 0.0 ] );
*
* dgbmv.ndarray( 'no-transpose', 3, 3, 0, 1, 1.0, A, 2, 1, 0, x, 1, 0, 0.0, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgbmv = main;
} else {
	dgbmv = tmp;
}


// EXPORTS //

module.exports = dgbmv;

// exports: { "ndarray": "dgbmv.ndarray" }
