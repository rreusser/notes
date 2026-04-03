'use strict';

/**
* Solves the generalized Sylvester equation using a level-3 blocked algorithm.
*
* @module @stdlib/lapack/base/ztgsyl
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var ztgsyl = require( '@stdlib/lapack/base/ztgsyl' );
*
* var A = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.5, 0.2, 2.0, -0.3 ] );
* var B = new Complex128Array( [ 3.0, 0.1, 0.0, 0.0, 0.3, -0.1, 4.0, 0.2 ] );
* var C = new Complex128Array( [ 1.0, 0.5, 3.0, 1.0, 2.0, -0.5, 4.0, 0.3 ] );
* var D = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.5, -0.1 ] );
* var E = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.05, 2.0, 0.1 ] );
* var F = new Complex128Array( [ 5.0, 1.0, 7.0, 0.5, 6.0, -1.0, 8.0, 0.2 ] );
* var scale = new Float64Array( 1 );
* var dif = new Float64Array( 1 );
* var IWORK = new Int32Array( 10 );
*
* var info = ztgsyl.ndarray( 'no-transpose', 0, 2, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, D, 1, 2, 0, E, 1, 2, 0, F, 1, 2, 0, scale, dif, null, 1, 0, -1, IWORK, 1, 0 );
* // C and F are overwritten with the solution
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztgsyl.ndarray" }
