

'use strict';

/**
* Compute the eigenvalues and eigenvectors of a 2-by-2 complex symmetric matrix.
*
* @module @stdlib/lapack/base/zlaesy
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

var zlaesy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaesy = main;
} else {
	zlaesy = tmp;
}


// EXPORTS //

module.exports = zlaesy;

// exports: { "ndarray": "zlaesy.ndarray" }
