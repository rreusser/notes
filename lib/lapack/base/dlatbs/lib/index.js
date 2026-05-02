
'use strict';

/**
* Solves a triangular banded system with scaling for overflow.
*
* @module @stdlib/lapack/base/dlatbs
*
*
* @example
* var dlatbs = require( '@stdlib/lapack/base/dlatbs' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var x = discreteUniform( N, -10, 10, opts );
* var CNORM = discreteUniform( N, -10, 10, opts );
*
* dlatbs.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', N, 1, AB, N, 1, 0, x, 1, 0, 1.0, CNORM, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlatbs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlatbs = main;
} else {
	dlatbs = tmp;
}


// EXPORTS //

module.exports = dlatbs;

// exports: { "ndarray": "dlatbs.ndarray" }
