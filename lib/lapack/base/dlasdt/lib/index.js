
'use strict';

/**
* Create a tree of subproblems for bidiagonal divide and conquer.
*
* @module @stdlib/lapack/base/dlasdt
*
*
* @example
* var dlasdt = require( '@stdlib/lapack/base/dlasdt' );
*
* var N = 3;
* var INODE = discreteUniform( N, -10, 10, opts );
* var NDIML = discreteUniform( N, -10, 10, opts );
* var NDIMR = discreteUniform( N, -10, 10, opts );
*
* dlasdt.ndarray( N, 1, 1, INODE, 1, 0, NDIML, 1, 0, NDIMR, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasdt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasdt = main;
} else {
	dlasdt = tmp;
}


// EXPORTS //

module.exports = dlasdt;

// exports: { "ndarray": "dlasdt.ndarray" }
