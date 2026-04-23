
'use strict';

/**
* Computes eigenvectors of a real upper quasi-triangular matrix.
*
* @module @stdlib/lapack/base/dtrevc3
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

var dtrevc3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrevc3 = main;
} else {
	dtrevc3 = tmp;
}


// EXPORTS //

module.exports = dtrevc3;

// exports: { "ndarray": "dtrevc3.ndarray" }
