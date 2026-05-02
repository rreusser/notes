
'use strict';

/**
* Take the square root of the overflow and underflow thresholds if the exponent range is very large.
*
* @module @stdlib/lapack/base/dlabad
*
*
* @example
* var dlabad = require( '@stdlib/lapack/base/dlabad' );
*
* dlabad.ndarray( 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlabad;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlabad = main;
} else {
	dlabad = tmp;
}


// EXPORTS //

module.exports = dlabad;

// exports: { "ndarray": "dlabad.ndarray" }
