
'use strict';

/**
* Add a vector into a doubled-single accumulation vector.
*
* @module @stdlib/lapack/base/dla_wwaddw
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

var dla_wwaddw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_wwaddw = main;
} else {
	dla_wwaddw = tmp;
}


// EXPORTS //

module.exports = dla_wwaddw;

// exports: { "ndarray": "dla_wwaddw.ndarray" }
