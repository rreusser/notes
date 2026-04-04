
'use strict';

/**
* Perform the Hermitian packed rank-1 update A := alpha_x_x**H + A.
*
* @module @stdlib/blas/base/zhpr
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

var zhpr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpr = main;
} else {
	zhpr = tmp;
}


// EXPORTS //

module.exports = zhpr;

// exports: { "ndarray": "zhpr.ndarray" }
