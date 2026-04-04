

'use strict';

/**
* Complex symmetric matrix norm
*
* @module @stdlib/lapack/base/zlansy
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

var zlansy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlansy = main;
} else {
	zlansy = tmp;
}


// EXPORTS //

module.exports = zlansy;

// exports: { "ndarray": "zlansy.ndarray" }
