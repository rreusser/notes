
'use strict';

/**
* Solves a complex banded system of linear equations A*X = B using LU factorization.
*
* @module @stdlib/lapack/base/zgbsv
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

var zgbsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbsv = main;
} else {
	zgbsv = tmp;
}


// EXPORTS //

module.exports = zgbsv;

// exports: { "ndarray": "zgbsv.ndarray" }
