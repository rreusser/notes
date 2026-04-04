
'use strict';

/**
* Estimate the reciprocal of the condition number of a real general tridiagonal matrix.
*
* @module @stdlib/lapack/base/dgtcon
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

var dgtcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtcon = main;
} else {
	dgtcon = tmp;
}


// EXPORTS //

module.exports = dgtcon;

// exports: { "ndarray": "dgtcon.ndarray" }
