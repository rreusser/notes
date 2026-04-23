

'use strict';

/**
* Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zlagtm
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

var zlagtm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlagtm = main;
} else {
	zlagtm = tmp;
}


// EXPORTS //

module.exports = zlagtm;

// exports: { "ndarray": "zlagtm.ndarray" }
