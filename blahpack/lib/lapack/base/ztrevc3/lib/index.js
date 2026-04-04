

'use strict';

/**
* Computes eigenvectors of a complex upper triangular matrix
*
* @module @stdlib/lapack/base/ztrevc3
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

var ztrevc3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrevc3 = main;
} else {
	ztrevc3 = tmp;
}


// EXPORTS //

module.exports = ztrevc3;

// exports: { "ndarray": "ztrevc3.ndarray" }
