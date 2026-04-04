
'use strict';

/**
* Perform reciprocal diagonal scaling on a complex matrix.
*
* @module @stdlib/lapack/base/zlarscl2
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

var zlarscl2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlarscl2 = main;
} else {
	zlarscl2 = tmp;
}


// EXPORTS //

module.exports = zlarscl2;

// exports: { "ndarray": "zlarscl2.ndarray" }
