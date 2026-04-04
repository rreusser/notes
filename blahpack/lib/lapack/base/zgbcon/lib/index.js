
'use strict';

/**
* Estimate reciprocal condition number of complex general band matrix.
*
* @module @stdlib/lapack/base/zgbcon
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

var zgbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbcon = main;
} else {
	zgbcon = tmp;
}


// EXPORTS //

module.exports = zgbcon;

// exports: { "ndarray": "zgbcon.ndarray" }
