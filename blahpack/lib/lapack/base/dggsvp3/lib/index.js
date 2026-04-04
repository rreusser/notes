
'use strict';

/**
* Compute the preprocessing for the generalized SVD of real matrices A and B.
*
* @module @stdlib/lapack/base/dggsvp3
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

var dggsvp3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dggsvp3 = main;
} else {
	dggsvp3 = tmp;
}


// EXPORTS //

module.exports = dggsvp3;

// exports: { "ndarray": "dggsvp3.ndarray" }
