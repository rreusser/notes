
'use strict';

/**
* Performs Hermitian matrix-matrix multiplication.
*
* @module @stdlib/blas/base/zhemm
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

var zhemm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhemm = main;
} else {
	zhemm = tmp;
}


// EXPORTS //

module.exports = zhemm;

// exports: { "ndarray": "zhemm.ndarray" }
