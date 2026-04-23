
'use strict';

/**
* Computes eigenvalues and Schur decomposition of a real general matrix.
*
* @module @stdlib/lapack/base/dgees
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

var dgees;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgees = main;
} else {
	dgees = tmp;
}


// EXPORTS //

module.exports = dgees;

// exports: { "ndarray": "dgees.ndarray" }
