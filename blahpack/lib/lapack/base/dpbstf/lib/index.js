
'use strict';

/**
* Computes a split Cholesky factorization of a real symmetric positive definite band matrix.
*
* @module @stdlib/lapack/base/dpbstf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpbstf = require( '@stdlib/lapack/base/dpbstf' );
*
* var ab = new Float64Array( [ 0.0, 2.0, -1.0, 2.0, -1.0, 2.0, -1.0, 2.0 ] );
* var info = dpbstf.ndarray( 'upper', 4, 1, ab, 1, 2, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpbstf.ndarray" }
