
'use strict';

/**
* Computes the Cholesky factorization with complete pivoting of a real symmetric positive semi-definite matrix (unblocked algorithm).
*
* @module @stdlib/lapack/base/dpstf2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpstf2 = require( '@stdlib/lapack/base/dpstf2' );
*
* var A = new Float64Array( [ 4.0, 2.0, 1.0, 2.0, 5.0, 3.0, 1.0, 3.0, 6.0 ] );
* var PIV = new Int32Array( 3 );
* var RANK = new Int32Array( 1 );
* var WORK = new Float64Array( 6 );
*
* dpstf2( 'column-major', 'upper', 3, A, 3, PIV, RANK, -1.0, WORK );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpstf2 = require( '@stdlib/lapack/base/dpstf2' );
*
* var A = new Float64Array( [ 4.0, 2.0, 1.0, 2.0, 5.0, 3.0, 1.0, 3.0, 6.0 ] );
* var PIV = new Int32Array( 3 );
* var RANK = new Int32Array( 1 );
* var WORK = new Float64Array( 6 );
*
* dpstf2.ndarray( 'upper', 3, A, 1, 3, 0, PIV, 1, 0, RANK, -1.0, WORK );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpstf2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpstf2 = main;
} else {
	dpstf2 = tmp;
}


// EXPORTS //

module.exports = dpstf2;

// exports: { "ndarray": "dpstf2.ndarray" }
