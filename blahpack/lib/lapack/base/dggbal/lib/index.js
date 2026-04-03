
'use strict';

/**
* Balances a pair of general real matrices (A,B) for the generalized eigenvalue problem.
*
* @module @stdlib/lapack/base/dggbal
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dggbal = require( './main.js' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
* var LSCALE = new Float64Array( 2 );
* var RSCALE = new Float64Array( 2 );
* var WORK = new Float64Array( 12 );
*
* var result = dggbal.ndarray( 'both', 2, A, 1, 2, 0, B, 1, 2, 0, LSCALE, 1, 0, RSCALE, 1, 0, WORK, 1, 0 );
* // returns { info: 0, ilo: 1, ihi: 2 }
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dggbal.ndarray" }
