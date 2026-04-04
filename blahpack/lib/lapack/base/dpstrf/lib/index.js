
'use strict';

/**
* Computes the Cholesky factorization with complete pivoting of a real symmetric positive semi-definite matrix (blocked algorithm).
*
* @module @stdlib/lapack/base/dpstrf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpstrf = require( '@stdlib/lapack/base/dpstrf' );
*
* var A = new Float64Array( [ 6.0, 3.0, 1.0, 3.0, 5.0, 2.0, 1.0, 2.0, 4.0 ] );
* var PIV = new Int32Array( 3 );
* var RANK = new Int32Array( 1 );
* var WORK = new Float64Array( 6 );
*
* dpstrf( 'column-major', 'upper', 3, A, 3, PIV, RANK, -1.0, WORK );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpstrf = require( '@stdlib/lapack/base/dpstrf' );
*
* var A = new Float64Array( [ 6.0, 3.0, 1.0, 3.0, 5.0, 2.0, 1.0, 2.0, 4.0 ] );
* var PIV = new Int32Array( 3 );
* var RANK = new Int32Array( 1 );
* var WORK = new Float64Array( 6 );
*
* dpstrf.ndarray( 'upper', 3, A, 1, 3, 0, PIV, 1, 0, RANK, -1.0, WORK );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpstrf.ndarray" }
