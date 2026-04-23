
'use strict';

/**
* Computes eigenvalues and eigenvectors of a complex general matrix.
*
* @module @stdlib/lapack/base/zgeev
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

var zgeev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgeev = main;
} else {
	zgeev = tmp;
}


// EXPORTS //

module.exports = zgeev;

// exports: { "ndarray": "zgeev.ndarray" }
