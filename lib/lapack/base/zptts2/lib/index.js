

'use strict';

/**
* Solves a complex Hermitian tridiagonal system using LDL^H factorization
*
* @module @stdlib/lapack/base/zptts2
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

var zptts2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zptts2 = main;
} else {
	zptts2 = tmp;
}


// EXPORTS //

module.exports = zptts2;

// exports: { "ndarray": "zptts2.ndarray" }
