

'use strict';

/**
* Equilibrate a complex general matrix using row and column scalings
*
* @module @stdlib/lapack/base/zlaqge
*
*
* @example
* var zlaqge = require( '@stdlib/lapack/base/zlaqge' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var r = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
*
* zlaqge.ndarray( N, N, A, N, 1, 0, r, 1, 0, c, 1, 0, 1, 1, 1, 'none' );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqge;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqge = main;
} else {
	zlaqge = tmp;
}


// EXPORTS //

module.exports = zlaqge;

// exports: { "ndarray": "zlaqge.ndarray" }
