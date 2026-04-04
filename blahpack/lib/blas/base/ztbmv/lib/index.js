
'use strict';

/**
* Perform one of the matrix-vector operations x := A_x, x := A__T_x, or x := A*_H_x, where A is a triangular band matrix.
*
* @module @stdlib/blas/base/ztbmv
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

var ztbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztbmv = main;
} else {
	ztbmv = tmp;
}


// EXPORTS //

module.exports = ztbmv;

// exports: { "ndarray": "ztbmv.ndarray" }
