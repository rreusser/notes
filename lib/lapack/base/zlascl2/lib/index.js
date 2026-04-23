

'use strict';

/**
* Perform diagonal scaling on a complex matrix.
*
* @module @stdlib/lapack/base/zlascl2
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

var zlascl2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlascl2 = main;
} else {
	zlascl2 = tmp;
}


// EXPORTS //

module.exports = zlascl2;

// exports: { "ndarray": "zlascl2.ndarray" }
