
'use strict';

/**
* Auxiliary bisection routine for tridiagonal eigenvalue computation.
*
* @module @stdlib/lapack/base/dlaebz
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

var dlaebz;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaebz = main;
} else {
	dlaebz = tmp;
}


// EXPORTS //

module.exports = dlaebz;

// exports: { "ndarray": "dlaebz.ndarray" }
