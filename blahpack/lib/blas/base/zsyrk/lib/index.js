
'use strict';

/**
* Perform one of the symmetric rank-k operations C := alpha_A_A**T + beta_C or C := alpha_A**T_A + beta_C.
*
* @module @stdlib/blas/base/zsyrk
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
