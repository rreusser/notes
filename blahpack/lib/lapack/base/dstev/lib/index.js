
'use strict';

/**
* Compute all eigenvalues and optionally eigenvectors of a real symmetric tridiagonal matrix.
*
* @module @stdlib/lapack/base/dstev
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

var dstev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstev = main;
} else {
	dstev = tmp;
}


// EXPORTS //

module.exports = dstev;

// exports: { "ndarray": "dstev.ndarray" }
