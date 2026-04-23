
'use strict';

/**
* Sets a scalar multiple of the first column of H - shift product.
*
* @module @stdlib/lapack/base/dlaqr1
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

var dlaqr1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr1 = main;
} else {
	dlaqr1 = tmp;
}


// EXPORTS //

module.exports = dlaqr1;

// exports: { "ndarray": "dlaqr1.ndarray" }
