
'use strict';

/**
* Compute the splitting points with threshold based on the representation.
*
* @module @stdlib/lapack/base/dlarra
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

var dlarra;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarra = main;
} else {
	dlarra = tmp;
}


// EXPORTS //

module.exports = dlarra;

// exports: { "ndarray": "dlarra.ndarray" }
