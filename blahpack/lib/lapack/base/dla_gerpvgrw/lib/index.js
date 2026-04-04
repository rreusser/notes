
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U).
*
* @module @stdlib/lapack/base/dla_gerpvgrw
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

var dla_gerpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_gerpvgrw = main;
} else {
	dla_gerpvgrw = tmp;
}


// EXPORTS //

module.exports = dla_gerpvgrw;

// exports: { "ndarray": "dla_gerpvgrw.ndarray" }
