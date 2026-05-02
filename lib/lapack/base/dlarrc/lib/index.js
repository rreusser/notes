
'use strict';

/**
* Count eigenvalues of a symmetric tridiagonal matrix in an interval.
*
* @module @stdlib/lapack/base/dlarrc
*
*
* @example
* var dlarrc = require( '@stdlib/lapack/base/dlarrc' );
*
* var N = 3;
* var D = discreteUniform( N, -10, 10, opts );
* var E = discreteUniform( N, -10, 10, opts );
*
* dlarrc.ndarray( 1, N, 1.0, 1.0, D, 1, 0, E, 1, 0, 1.0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarrc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarrc = main;
} else {
	dlarrc = tmp;
}


// EXPORTS //

module.exports = dlarrc;

// exports: { "ndarray": "dlarrc.ndarray" }
