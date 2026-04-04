
'use strict';

/**
* Estimates the reciprocal condition number of a general banded matrix.
*
* @module @stdlib/lapack/base/dgbcon
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

var dgbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgbcon = main;
} else {
	dgbcon = tmp;
}


// EXPORTS //

module.exports = dgbcon;

// exports: { "ndarray": "dgbcon.ndarray" }
