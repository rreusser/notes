
'use strict';

/**
* Solve one of the triangular packed systems A_x = b or A__T_x = b or A*_H_x = b.
*
* @module @stdlib/blas/base/ztpsv
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

var ztpsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpsv = main;
} else {
	ztpsv = tmp;
}


// EXPORTS //

module.exports = ztpsv;

// exports: { "ndarray": "ztpsv.ndarray" }
