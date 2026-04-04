
'use strict';

/**
* Estimates the reciprocal condition number of a triangular matrix.
*
* @module @stdlib/lapack/base/dtrcon
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

var dtrcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrcon = main;
} else {
	dtrcon = tmp;
}


// EXPORTS //

module.exports = dtrcon;

// exports: { "ndarray": "dtrcon.ndarray" }
