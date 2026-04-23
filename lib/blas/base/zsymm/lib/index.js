
'use strict';

/**
* Perform one of the symmetric matrix-matrix operations C := alpha_A_B + beta_C or C := alpha_B_A + beta_C.
*
* @module @stdlib/blas/base/zsymm
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

var zsymm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsymm = main;
} else {
	zsymm = tmp;
}


// EXPORTS //

module.exports = zsymm;

// exports: { "ndarray": "zsymm.ndarray" }
