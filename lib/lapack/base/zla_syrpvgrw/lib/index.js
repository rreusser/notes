
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a complex symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/zla_syrpvgrw
*
*
* @example
* var zla_syrpvgrw = require( '@stdlib/lapack/base/zla_syrpvgrw' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zla_syrpvgrw.ndarray( 'upper', N, 1, A, N, 1, 0, AF, N, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zla_syrpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_syrpvgrw = main;
} else {
	zla_syrpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_syrpvgrw;

// exports: { "ndarray": "zla_syrpvgrw.ndarray" }
