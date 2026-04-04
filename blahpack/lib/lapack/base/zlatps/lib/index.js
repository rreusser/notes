
'use strict';

/**
* Solves a complex triangular system with scaling to prevent overflow, where the matrix is in packed storage.
*
* @module @stdlib/lapack/base/zlatps
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlatps = require( '@stdlib/lapack/base/zlatps' );
*
* var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.5, 3.0, 0.0 ] );
* var x = new Complex128Array( [ 4.0, 2.0, 6.0, 0.0 ] );
* var scale = new Float64Array( 1 );
* var cnorm = new Float64Array( 2 );
*
* var info = zlatps.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', 2, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlatps.ndarray" }
