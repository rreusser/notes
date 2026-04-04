
'use strict';

/**
* Equilibrate a general band matrix using row and column scaling factors.
*
* @module @stdlib/lapack/base/dlaqgb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlaqgb = require( '@stdlib/lapack/base/dlaqgb' );
*
* var AB = new Float64Array( [ 0.0, 0.0, 2.0, 1.5, 0.0, 3.0, 1.0, 0.5, 0.8, 2.5, 4.0, 1.2, 0.6, 3.5, 2.0, 0.0, 1.0, 0.7, 0.0, 0.0 ] );
* var r = new Float64Array( [ 0.5, 1.0, 0.8, 0.25 ] );
* var c = new Float64Array( [ 0.6, 1.0, 0.7, 0.9, 0.4 ] );
*
* var equed = dlaqgb.ndarray( 4, 5, 1, 2, AB, 1, 4, 0, r, 1, 0, c, 1, 0, 0.01, 0.01, 4.0 );
* // returns 'both'
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlaqgb.ndarray" }
