

'use strict';

/**
* Complex triangular banded solve with scaling
*
* @module @stdlib/lapack/base/zlatbs
*
*
* @example
* var zlatbs = require( '@stdlib/lapack/base/zlatbs' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var x = discreteUniform( N, -10, 10, opts );
* var CNORM = discreteUniform( N, -10, 10, opts );
*
* zlatbs.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', N, 1, AB, N, 1, 0, x, 1, 0, 1.0, CNORM, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlatbs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlatbs = main;
} else {
	zlatbs = tmp;
}


// EXPORTS //

module.exports = zlatbs;

// exports: { "ndarray": "zlatbs.ndarray" }
