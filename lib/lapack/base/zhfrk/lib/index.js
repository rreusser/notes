
'use strict';

/**
* Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/zhfrk
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zhfrk = require( '@stdlib/lapack/base/zhfrk' );
*
* var A = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 1.0, 1.0, 2.0, 1.0, 3.0, 1.0 ] );
* var C = new Complex128Array( 6 );
*
* zhfrk.ndarray( 'no-transpose', 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhfrk;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhfrk = main;
} else {
	zhfrk = tmp;
}


// EXPORTS //

module.exports = zhfrk;

// exports: { "ndarray": "zhfrk.ndarray" }
