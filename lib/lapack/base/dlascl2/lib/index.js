
'use strict';

/**
* Perform diagonal scaling on a matrix.
*
* @module @stdlib/lapack/base/dlascl2
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

var dlascl2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlascl2 = main;
} else {
	dlascl2 = tmp;
}


// EXPORTS //

module.exports = dlascl2;

// exports: { "ndarray": "dlascl2.ndarray" }
