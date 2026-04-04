
'use strict';

/**
* Computes the generalized SVD of two upper triangular matrices via Jacobi-Kogbetliantz iteration.
*
* @module @stdlib/lapack/base/dtgsja
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

var dtgsja;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtgsja = main;
} else {
	dtgsja = tmp;
}


// EXPORTS //

module.exports = dtgsja;

// exports: { "ndarray": "dtgsja.ndarray" }
