

'use strict';

/**
* Equilibrate a Hermitian matrix using scaling factors
*
* @module @stdlib/lapack/base/zlaqhe
*
*
* @example
* var zlaqhe = require( '@stdlib/lapack/base/zlaqhe' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* zlaqhe.ndarray( 'upper', N, A, N, 1, 0, s, 1, 0, 1, 1, 'none' );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqhe;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqhe = main;
} else {
	zlaqhe = tmp;
}


// EXPORTS //

module.exports = zlaqhe;

// exports: { "ndarray": "zlaqhe.ndarray" }
