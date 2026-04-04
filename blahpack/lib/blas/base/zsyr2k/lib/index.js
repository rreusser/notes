
'use strict';

/**
* Perform one of the symmetric rank-2k operations C := alpha_A_B**T + alpha_B_A**T + beta_C or C := alpha_A**T_B + alpha_B**T_A + beta_C.
*
* @module @stdlib/blas/base/zsyr2k
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

var zsyr2k;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsyr2k = main;
} else {
	zsyr2k = tmp;
}


// EXPORTS //

module.exports = zsyr2k;

// exports: { "ndarray": "zsyr2k.ndarray" }
