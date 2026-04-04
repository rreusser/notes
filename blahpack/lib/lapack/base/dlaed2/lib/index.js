'use strict';

/**
* Merge eigenvalues and deflate secular equation in divide and conquer.
*
* @module @stdlib/lapack/base/dlaed2
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

var dlaed2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaed2 = main;
} else {
	dlaed2 = tmp;
}


// EXPORTS //

module.exports = dlaed2;

// exports: { "ndarray": "dlaed2.ndarray" }
