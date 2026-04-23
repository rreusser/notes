

'use strict';

/**
* Estimate the reciprocal condition number of a complex triangular matrix
*
* @module @stdlib/lapack/base/ztrcon
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

var ztrcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrcon = main;
} else {
	ztrcon = tmp;
}


// EXPORTS //

module.exports = ztrcon;

// exports: { "ndarray": "ztrcon.ndarray" }
