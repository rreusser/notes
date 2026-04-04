
'use strict';

/**
* LU factorization with complete pivoting of a general NxN matrix.
*
* @module @stdlib/lapack/base/dgetc2
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

var dgetc2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgetc2 = main;
} else {
	dgetc2 = tmp;
}


// EXPORTS //

module.exports = dgetc2;

// exports: { "ndarray": "dgetc2.ndarray" }
