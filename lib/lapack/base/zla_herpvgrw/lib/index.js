
'use strict';

/**
* Compute the reciprocal pivot growth factor for a complex Hermitian indefinite matrix.
*
* @module @stdlib/lapack/base/zla_herpvgrw
*
*
* @example
* var zla_herpvgrw = require( '@stdlib/lapack/base/zla_herpvgrw' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zla_herpvgrw.ndarray( 'upper', N, 1, A, N, 1, 0, AF, N, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zla_herpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_herpvgrw = main;
} else {
	zla_herpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_herpvgrw;

// exports: { "ndarray": "zla_herpvgrw.ndarray" }
