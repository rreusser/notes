
'use strict';

/**
* Solves a general real tridiagonal system of linear equations.
*
* @module @stdlib/lapack/base/dgtsv
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

var dgtsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtsv = main;
} else {
	dgtsv = tmp;
}


// EXPORTS //

module.exports = dgtsv;

// exports: { "ndarray": "dgtsv.ndarray" }
