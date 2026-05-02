
'use strict';

/**
* Perform one of the symmetric rank-k operations C := alpha_A_A**T + beta_C or C := alpha_A**T_A + beta_C.
*
* @module @stdlib/blas/base/zsyrk
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zsyrk = require( '@stdlib/blas/base/zsyrk' );
*
* var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var C = new Float64Array( 8 );
* var alpha = new Float64Array( [ 1.0, 0.0 ] );
* var beta = new Float64Array( [ 0.0, 0.0 ] );
*
* zsyrk.ndarray( 'upper', 'no-transpose', 2, 2, alpha, A, 2, 1, 0, beta, C, 2, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsyrk;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsyrk = main;
} else {
	zsyrk = tmp;
}


// EXPORTS //

module.exports = zsyrk;

// exports: { "ndarray": "zsyrk.ndarray" }
