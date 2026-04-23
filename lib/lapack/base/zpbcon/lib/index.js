

'use strict';

/**
* Estimate reciprocal condition number of complex positive definite band matrix
*
* @module @stdlib/lapack/base/zpbcon
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

var zpbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpbcon = main;
} else {
	zpbcon = tmp;
}


// EXPORTS //

module.exports = zpbcon;

// exports: { "ndarray": "zpbcon.ndarray" }
