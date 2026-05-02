
'use strict';

/**
* Rearrange the rows of a matrix as specified by a permutation vector.
*
* @module @stdlib/lapack/base/dlapmr
*
*
* @example
* var dlapmr = require( '@stdlib/lapack/base/dlapmr' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var k = discreteUniform( N, -10, 10, opts );
*
* dlapmr.ndarray( 1, N, N, X, N, 1, 0, k, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlapmr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlapmr = main;
} else {
	dlapmr = tmp;
}


// EXPORTS //

module.exports = dlapmr;

// exports: { "ndarray": "dlapmr.ndarray" }
