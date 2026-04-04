

'use strict';

/**
* Complex symmetric matrix-vector multiply
*
* @module @stdlib/lapack/base/zsymv
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

var zsymv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsymv = main;
} else {
	zsymv = tmp;
}


// EXPORTS //

module.exports = zsymv;

// exports: { "ndarray": "zsymv.ndarray" }
