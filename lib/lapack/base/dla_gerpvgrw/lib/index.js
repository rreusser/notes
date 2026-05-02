
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U).
*
* @module @stdlib/lapack/base/dla_gerpvgrw
*
*
* @example
* var dla_gerpvgrw = require( '@stdlib/lapack/base/dla_gerpvgrw' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
*
* dla_gerpvgrw.ndarray( N, 1, A, N, 1, 0, AF, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dla_gerpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_gerpvgrw = main;
} else {
	dla_gerpvgrw = tmp;
}


// EXPORTS //

module.exports = dla_gerpvgrw;

// exports: { "ndarray": "dla_gerpvgrw.ndarray" }
