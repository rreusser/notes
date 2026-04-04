
'use strict';

/**
* Computes eigenvalues and optionally eigenvectors of a generalized symmetric-definite eigenproblem.
*
* @module @stdlib/lapack/base/dsygv
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

var dsygv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygv = main;
} else {
	dsygv = tmp;
}


// EXPORTS //

module.exports = dsygv;

// exports: { "ndarray": "dsygv.ndarray" }
