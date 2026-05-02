
'use strict';

/**
* Compute the splitting points with threshold based on the representation.
*
* @module @stdlib/lapack/base/dlarra
*
*
* @example
* var dlarra = require( '@stdlib/lapack/base/dlarra' );
*
* var N = 3;
* var e = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N * N, -10, 10, opts );
* var ISPLIT = discreteUniform( N, -10, 10, opts );
*
* dlarra.ndarray( N, d, 1, 0, e, 1, 0, 1, 1, 0, 1, 1, 1, ISPLIT, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarra;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarra = main;
} else {
	dlarra = tmp;
}


// EXPORTS //

module.exports = dlarra;

// exports: { "ndarray": "dlarra.ndarray" }
