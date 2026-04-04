
'use strict';

/**
* Merge two sets of singular values in bidiagonal SVD divide and conquer.
*
* @module @stdlib/lapack/base/dlasd2
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

var dlasd2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasd2 = main;
} else {
	dlasd2 = tmp;
}


// EXPORTS //

module.exports = dlasd2;

// exports: { "ndarray": "dlasd2.ndarray" }
