
'use strict';

/**
* Solves the real Sylvester matrix equation.
*
* @module @stdlib/lapack/base/dtrsyl
*
*
* @example
* var dtrsyl = require( '@stdlib/lapack/base/dtrsyl' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
*
* dtrsyl.ndarray( 1, 1, 1, N, N, A, N, 1, 0, B, N, 1, 0, C, N, 1, 0, 1.0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrsyl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrsyl = main;
} else {
	dtrsyl = tmp;
}


// EXPORTS //

module.exports = dtrsyl;

// exports: { "ndarray": "dtrsyl.ndarray" }
