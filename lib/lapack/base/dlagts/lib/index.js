
'use strict';

/**
* Solves a tridiagonal system factored by dlagtf.
*
* @module @stdlib/lapack/base/dlagts
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

var dlagts;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlagts = main;
} else {
	dlagts = tmp;
}


// EXPORTS //

module.exports = dlagts;

// exports: { "ndarray": "dlagts.ndarray" }
