
'use strict';

/**
* Perform the Hermitian packed rank-2 update A := alpha_x_y**H + conj(alpha)_y_x**H + A.
*
* @module @stdlib/blas/base/zhpr2
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

var zhpr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpr2 = main;
} else {
	zhpr2 = tmp;
}


// EXPORTS //

module.exports = zhpr2;

// exports: { "ndarray": "zhpr2.ndarray" }
