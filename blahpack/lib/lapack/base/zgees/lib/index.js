
'use strict';

/**
* Compute eigenvalues and Schur decomposition of a complex matrix.
*
* @module @stdlib/lapack/base/zgees
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

var zgees;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgees = main;
} else {
	zgees = tmp;
}


// EXPORTS //

module.exports = zgees;

// exports: { "ndarray": "zgees.ndarray" }
