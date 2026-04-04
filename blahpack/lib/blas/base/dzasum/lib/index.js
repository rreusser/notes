
'use strict';

/**
* Compute the sum of absolute values of a complex vector.
*
* @module @stdlib/blas/base/dzasum
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

var dzasum;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dzasum = main;
} else {
	dzasum = tmp;
}


// EXPORTS //

module.exports = dzasum;

// exports: { "ndarray": "dzasum.ndarray" }
