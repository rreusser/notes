
'use strict';

/**
* Compute all eigenvalues and optionally eigenvectors of a real symmetric tridiagonal matrix.
*
* @module @stdlib/lapack/base/dstev
*
*
* @example
* var dstev = require( '@stdlib/lapack/base/dstev' );
*
* var N = 3;
* var Z = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dstev.ndarray( 'compute-vectors', N, d, 1, 0, e, 1, 0, Z, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dstev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstev = main;
} else {
	dstev = tmp;
}


// EXPORTS //

module.exports = dstev;

// exports: { "ndarray": "dstev.ndarray" }
