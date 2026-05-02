
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a complex general matrix.
*
* @module @stdlib/lapack/base/zla_gerpvgrw
*
*
* @example
* var zla_gerpvgrw = require( '@stdlib/lapack/base/zla_gerpvgrw' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
*
* zla_gerpvgrw.ndarray( N, 1, A, N, 1, 0, AF, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zla_gerpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_gerpvgrw = main;
} else {
	zla_gerpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_gerpvgrw;

// exports: { "ndarray": "zla_gerpvgrw.ndarray" }
