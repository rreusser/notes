
'use strict';

/**
* Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection.
*
* @module @stdlib/lapack/base/dstebz
*
*
* @example
* var dstebz = require( '@stdlib/lapack/base/dstebz' );
*
* var N = 3;
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var IBLOCK = discreteUniform( N, -10, 10, opts );
* var ISPLIT = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dstebz.ndarray( 'all', 'row-major', N, 1.0, 1.0, N, N, 1.0, d, 1, 0, e, 1, 0, N, 1, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dstebz;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstebz = main;
} else {
	dstebz = tmp;
}


// EXPORTS //

module.exports = dstebz;

// exports: { "ndarray": "dstebz.ndarray" }
