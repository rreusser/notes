
'use strict';

/**
* Solve a complex general tridiagonal system of linear equations A * X = B.
*
* @module @stdlib/lapack/base/zgtsv
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

var zgtsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtsv = main;
} else {
	zgtsv = tmp;
}


// EXPORTS //

module.exports = zgtsv;

// exports: { "ndarray": "zgtsv.ndarray" }
