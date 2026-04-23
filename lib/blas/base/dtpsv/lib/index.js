
'use strict';

/**
* Solve a triangular packed system of equations.
*
* @module @stdlib/blas/base/dtpsv
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

var dtpsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpsv = main;
} else {
	dtpsv = tmp;
}


// EXPORTS //

module.exports = dtpsv;

// exports: { "ndarray": "dtpsv.ndarray" }
