
'use strict';

/**
* Perform one of the matrix-vector operations y := alpha_A_x + beta_y or y := alpha_A**T_x + beta_y or y := alpha*A**H_x + beta_y where A is a general band matrix.
*
* @module @stdlib/blas/base/zgbmv
*
* @example
* // TODO: Add example
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbmv = main;
} else {
	zgbmv = tmp;
}


// EXPORTS //

module.exports = zgbmv;

// exports: { "ndarray": "zgbmv.ndarray" }
