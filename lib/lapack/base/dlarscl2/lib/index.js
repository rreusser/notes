
'use strict';

/**
* Perform reciprocal diagonal scaling on a matrix.
*
* @module @stdlib/lapack/base/dlarscl2
*
*
* @example
* var dlarscl2 = require( '@stdlib/lapack/base/dlarscl2' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
*
* dlarscl2.ndarray( N, N, d, 1, 0, X, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarscl2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarscl2 = main;
} else {
	dlarscl2 = tmp;
}


// EXPORTS //

module.exports = dlarscl2;

// exports: { "ndarray": "dlarscl2.ndarray" }
