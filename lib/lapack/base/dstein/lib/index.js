
'use strict';

/**
* Compute eigenvectors of a real symmetric tridiagonal matrix by inverse iteration.
*
* @module @stdlib/lapack/base/dstein
*
*
* @example
* var dstein = require( '@stdlib/lapack/base/dstein' );
*
* var N = 3;
* var Z = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var IBLOCK = discreteUniform( N, -10, 10, opts );
* var ISPLIT = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
* var IFAIL = discreteUniform( N, -10, 10, opts );
*
* dstein.ndarray( N, d, 1, 0, e, 1, 0, N, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, Z, N, 1, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dstein;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstein = main;
} else {
	dstein = tmp;
}


// EXPORTS //

module.exports = dstein;

// exports: { "ndarray": "dstein.ndarray" }
