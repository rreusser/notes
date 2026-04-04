

'use strict';

/**
* Perform the symmetric packed matrix-vector operation y := alpha*A*x + beta*y where A is a complex symmetric matrix stored in packed format
*
* @module @stdlib/lapack/base/zspmv
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

var zspmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zspmv = main;
} else {
	zspmv = tmp;
}


// EXPORTS //

module.exports = zspmv;

// exports: { "ndarray": "zspmv.ndarray" }
