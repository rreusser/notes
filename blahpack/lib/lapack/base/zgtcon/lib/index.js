
'use strict';

/**
* Estimate reciprocal condition number of complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zgtcon
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

var zgtcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtcon = main;
} else {
	zgtcon = tmp;
}


// EXPORTS //

module.exports = zgtcon;

// exports: { "ndarray": "zgtcon.ndarray" }
