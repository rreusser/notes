

'use strict';

/**
* Computes eigenvalues and eigenvectors of a real general matrix
*
* @module @stdlib/lapack/base/dgeev
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

var dgeev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeev = main;
} else {
	dgeev = tmp;
}


// EXPORTS //

module.exports = dgeev;

// exports: { "ndarray": "dgeev.ndarray" }
