
'use strict';

/**
* Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix.
*
* @module @stdlib/lapack/base/dlasd5
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

var dlasd5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasd5 = main;
} else {
	dlasd5 = tmp;
}


// EXPORTS //

module.exports = dlasd5;

// exports: { "ndarray": "dlasd5.ndarray" }
