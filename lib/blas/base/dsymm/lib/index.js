
'use strict';

/**
* Performs symmetric matrix-matrix multiplication.
*
* @module @stdlib/blas/base/dsymm
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsymm = require( '@stdlib/blas/base/dsymm' );
*
* var A = new Float64Array( [ 1.0, 2.0, 2.0, 3.0 ] );
* var B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
* var C = new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] );
*
* dsymm.ndarray( 'left', 'upper', 2, 2, 1.0, A, 2, 1, 0, B, 2, 1, 0, 0.0, C, 2, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsymm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsymm = main;
} else {
	dsymm = tmp;
}


// EXPORTS //

module.exports = dsymm;

// exports: { "ndarray": "dsymm.ndarray" }
