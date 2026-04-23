
'use strict';

/**
* Multiply a general tridiagonal matrix by a rectangular matrix.
*
* @module @stdlib/lapack/base/dlagtm
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

var dlagtm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlagtm = main;
} else {
	dlagtm = tmp;
}


// EXPORTS //

module.exports = dlagtm;

// exports: { "ndarray": "dlagtm.ndarray" }
