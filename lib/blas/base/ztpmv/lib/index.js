
'use strict';

/**
* Perform one of the triangular packed matrix-vector operations x := A_x or x := A__T_x or x := A*_H_x.
*
* @module @stdlib/blas/base/ztpmv
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

var ztpmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpmv = main;
} else {
	ztpmv = tmp;
}


// EXPORTS //

module.exports = ztpmv;

// exports: { "ndarray": "ztpmv.ndarray" }
