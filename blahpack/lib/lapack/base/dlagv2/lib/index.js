
'use strict';

/**
* Computes the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular.
*
* @module @stdlib/lapack/base/dlagv2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlagv2 = require( './main.js' );
*
* var A = new Float64Array( [ 4.0, 2.0, 1.0, 3.0 ] );
* var B = new Float64Array( [ 2.0, 0.0, 1.0, 1.0 ] );
* var alphar = new Float64Array( 2 );
* var alphai = new Float64Array( 2 );
* var beta = new Float64Array( 2 );
*
* var result = dlagv2.ndarray( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 );
* // returns {...}
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlagv2.ndarray" }
