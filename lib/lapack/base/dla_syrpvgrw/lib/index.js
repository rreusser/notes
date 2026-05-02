
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/dla_syrpvgrw
*
*
* @example
* var dla_syrpvgrw = require( '@stdlib/lapack/base/dla_syrpvgrw' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dla_syrpvgrw.ndarray( 'upper', N, 1, A, N, 1, 0, AF, N, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dla_syrpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_syrpvgrw = main;
} else {
	dla_syrpvgrw = tmp;
}


// EXPORTS //

module.exports = dla_syrpvgrw;

// exports: { "ndarray": "dla_syrpvgrw.ndarray" }
