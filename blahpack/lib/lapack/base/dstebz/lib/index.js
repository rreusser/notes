
'use strict';

/**
* Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection.
*
* @module @stdlib/lapack/base/dstebz
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

var dstebz;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstebz = main;
} else {
	dstebz = tmp;
}


// EXPORTS //

module.exports = dstebz;

// exports: { "ndarray": "dstebz.ndarray" }
