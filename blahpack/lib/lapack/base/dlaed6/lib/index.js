
'use strict';

/**
* Compute one Newton step in the solution of the secular equation.
*
* @module @stdlib/lapack/base/dlaed6
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

var dlaed6;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaed6 = main;
} else {
	dlaed6 = tmp;
}


// EXPORTS //

module.exports = dlaed6;

// exports: { "ndarray": "dlaed6.ndarray" }
