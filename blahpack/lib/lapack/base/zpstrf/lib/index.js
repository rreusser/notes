
'use strict';

/**
* Computes the Cholesky factorization with complete pivoting of a complex Hermitian positive semi-definite matrix using a blocked algorithm.
*
* @module @stdlib/lapack/base/zpstrf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zpstrf = require( '@stdlib/lapack/base/zpstrf' );
*
* var A = new Complex128Array( [ 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 1.0, 8.0, 0.0, 0.0, 0.0, 3.0, -2.0, 1.0, 1.0, 6.0, 0.0 ] );
* var PIV = new Int32Array( 3 );
* var RANK = new Int32Array( 1 );
* var WORK = new Float64Array( 6 );
*
* zpstrf( 'column-major', 'upper', 3, A, 3, PIV, RANK, -1.0, WORK );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zpstrf = require( '@stdlib/lapack/base/zpstrf' );
*
* var A = new Complex128Array( [ 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 1.0, 8.0, 0.0, 0.0, 0.0, 3.0, -2.0, 1.0, 1.0, 6.0, 0.0 ] );
* var PIV = new Int32Array( 3 );
* var RANK = new Int32Array( 1 );
* var WORK = new Float64Array( 6 );
*
* zpstrf.ndarray( 'upper', 3, A, 1, 3, 0, PIV, 1, 0, RANK, -1.0, WORK );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpstrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpstrf = main;
} else {
	zpstrf = tmp;
}


// EXPORTS //

module.exports = zpstrf;

// exports: { "ndarray": "zpstrf.ndarray" }
