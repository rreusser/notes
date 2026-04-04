
'use strict';

/**
* Solves a complex system A * X = B where A is Hermitian positive definite band, with equilibration, condition estimation, and error bounds.
*
* @module @stdlib/lapack/base/zpbsvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zpbsvx = require( '@stdlib/lapack/base/zpbsvx' );
*
* var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, -1.0, 0.5, 4.0, 0.0, -1.0, 0.5, 4.0, 0.0 ] );
* var AFB = new Complex128Array( 6 );
* var S = new Float64Array( 3 );
* var equed = [ 'none' ];
* var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.0 ] );
* var X = new Complex128Array( 3 );
* var rcond = new Float64Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = zpbsvx.ndarray( 'not-factored', 'upper', 3, 1, 1, AB, 1, 2, 0, AFB, 1, 2, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zpbsvx.ndarray" }
