
'use strict';

/**
* Estimates the reciprocal of the condition number of a complex symmetric matrix in packed storage.
*
* @module @stdlib/lapack/base/zspcon
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zspcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zspcon = main;
} else {
	zspcon = tmp;
}


// EXPORTS //

module.exports = zspcon;

// exports: { "ndarray": "zspcon.ndarray" }
