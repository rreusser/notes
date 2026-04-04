
'use strict';

/**
* Compute eigenvectors of a real symmetric tridiagonal matrix by inverse iteration.
*
* @module @stdlib/lapack/base/dstein
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

var dstein;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstein = main;
} else {
	dstein = tmp;
}


// EXPORTS //

module.exports = dstein;

// exports: { "ndarray": "dstein.ndarray" }
