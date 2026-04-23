
'use strict';

/**
* Factorizes the matrix (T - lambda*I) where T is a tridiagonal matrix.
*
* @module @stdlib/lapack/base/dlagtf
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

var dlagtf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlagtf = main;
} else {
	dlagtf = tmp;
}


// EXPORTS //

module.exports = dlagtf;

// exports: { "ndarray": "dlagtf.ndarray" }
