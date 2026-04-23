
'use strict';

/**
* Computes contribution to reciprocal DIF estimate using LU factorization from dgetc2.
*
* @module @stdlib/lapack/base/dlatdf
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

var dlatdf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlatdf = main;
} else {
	dlatdf = tmp;
}


// EXPORTS //

module.exports = dlatdf;

// exports: { "ndarray": "dlatdf.ndarray" }
