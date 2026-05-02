
'use strict';

/**
* Perform matrix-vector operation with a triangular packed matrix.
*
* @module @stdlib/blas/base/dtpmv
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtpmv = require( '@stdlib/blas/base/dtpmv' );
*
* var AP = new Float64Array( [ 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 1.0 ] );
*
* dtpmv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, AP, 1, 0, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtpmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpmv = main;
} else {
	dtpmv = tmp;
}


// EXPORTS //

module.exports = dtpmv;

// exports: { "ndarray": "dtpmv.ndarray" }
