'use strict';

/**
* Performs a symmetric rank-k operation for a matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/dsfrk
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsfrk = require( '@stdlib/lapack/base/dsfrk' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
* var C = new Float64Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
*
* dsfrk.ndarray( 'no-transpose', 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 0 );
* // C => Float64Array [ 17, 22, 27, 45, 29, 36 ]
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsfrk;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsfrk = main;
} else {
	dsfrk = tmp;
}


// EXPORTS //

module.exports = dsfrk;

// exports: { "ndarray": "dsfrk.ndarray" }
