

'use strict';

/**
* Solves a complex triangular system with scaling to prevent overflow
*
* @module @stdlib/lapack/base/zlatrs
*
*
* @example
* var zlatrs = require( '@stdlib/lapack/base/zlatrs' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var x = discreteUniform( N, -10, 10, opts );
* var CNORM = discreteUniform( N, -10, 10, opts );
*
* zlatrs.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', N, A, N, 1, 0, x, 1, 0, 1.0, CNORM, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlatrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlatrs = main;
} else {
	zlatrs = tmp;
}


// EXPORTS //

module.exports = zlatrs;

// exports: { "ndarray": "zlatrs.ndarray" }
