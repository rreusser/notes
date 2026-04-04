'use strict';

/**
* Solves the generalized Sylvester matrix equation for small subsystems.
*
* @module @stdlib/lapack/base/ztgsy2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var ztgsy2 = require( '@stdlib/lapack/base/ztgsy2' );
*
* var A = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.5, 0.2, 2.0, -0.3 ] );
* var B = new Complex128Array( [ 3.0, 0.1, 0.0, 0.0, 0.3, -0.1, 4.0, 0.2 ] );
* var C = new Complex128Array( [ 1.0, 0.5, 3.0, 1.0, 2.0, -0.5, 4.0, 0.3 ] );
* var D = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.5, -0.1 ] );
* var E = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.05, 2.0, 0.1 ] );
* var F = new Complex128Array( [ 5.0, 1.0, 7.0, 0.5, 6.0, -1.0, 8.0, 0.2 ] );
* var scale = new Float64Array( 1 );
* var rdsum = new Float64Array( [ 0.0 ] );
* var rdscal = new Float64Array( [ 1.0 ] );
*
* var info = ztgsy2.ndarray( 'no-transpose', 0, 2, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, D, 1, 2, 0, E, 1, 2, 0, F, 1, 2, 0, scale, rdsum, rdscal );
* // C and F are overwritten with the solution
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztgsy2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztgsy2 = main;
} else {
	ztgsy2 = tmp;
}


// EXPORTS //

module.exports = ztgsy2;

// exports: { "ndarray": "ztgsy2.ndarray" }
