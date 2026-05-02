/* eslint-disable camelcase */

'use strict';

/**
* Compute the reciprocal pivot growth factor `norm(A)/norm(U)` for a complex Hermitian positive-definite matrix.
*
* @module @stdlib/lapack/base/zla_porpvgrw
*
*
* @example
* var zla_porpvgrw = require( '@stdlib/lapack/base/zla_porpvgrw' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zla_porpvgrw.ndarray( 'upper', 1, A, N, 1, 0, AF, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zla_porpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_porpvgrw = main;
} else {
	zla_porpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_porpvgrw;

// exports: { "ndarray": "zla_porpvgrw.ndarray" }
