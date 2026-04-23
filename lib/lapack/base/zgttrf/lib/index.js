
'use strict';

/**
* Compute LU factorization of complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zgttrf
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

var zgttrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgttrf = main;
} else {
	zgttrf = tmp;
}


// EXPORTS //

module.exports = zgttrf;

// exports: { "ndarray": "zgttrf.ndarray" }
