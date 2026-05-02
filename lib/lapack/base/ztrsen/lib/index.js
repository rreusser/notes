

'use strict';

/**
* Reorder Schur factorization and compute condition numbers
*
* @module @stdlib/lapack/base/ztrsen
*
*
* @example
* var ztrsen = require( '@stdlib/lapack/base/ztrsen' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
* var SELECT = discreteUniform( N, -10, 10, opts );
* var W = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* ztrsen.ndarray( 'both', 'update', SELECT, 1, 0, N, T, N, 1, 0, Q, N, 1, 0, W, 1, 0, N, 1.0, 1, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrsen;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrsen = main;
} else {
	ztrsen = tmp;
}


// EXPORTS //

module.exports = ztrsen;

// exports: { "ndarray": "ztrsen.ndarray" }
