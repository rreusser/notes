
'use strict';

/**
* Perform matrix-vector operation with a triangular band matrix.
*
* @module @stdlib/blas/base/dtbmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtbmv = require( '@stdlib/blas/base/dtbmv' );
*
* var A = new Float64Array( [ 0.0, 1.0, 2.0, 3.0 ] );
* var x = new Float64Array( [ 1.0, 1.0 ] );
*
* dtbmv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, 1, A, 2, 1, 0, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtbmv = main;
} else {
	dtbmv = tmp;
}


// EXPORTS //

module.exports = dtbmv;

// exports: { "ndarray": "dtbmv.ndarray" }
