'use strict';

/**
* Merge eigenvalues and deflate secular equation in divide and conquer.
*
* @module @stdlib/lapack/base/dlaed2
*
*
* @example
* var dlaed2 = require( '@stdlib/lapack/base/dlaed2' );
*
* var N = 3;
* var Q = discreteUniform( N * N, -10, 10, opts );
* var Q2 = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var INDXQ = discreteUniform( N, -10, 10, opts );
* var z = discreteUniform( N, -10, 10, opts );
* var DLAMBDA = discreteUniform( N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var INDX = discreteUniform( N, -10, 10, opts );
* var INDXC = discreteUniform( N, -10, 10, opts );
* var INDXP = discreteUniform( N, -10, 10, opts );
* var COLTYP = discreteUniform( N, -10, 10, opts );
*
* dlaed2.ndarray( N, 1, d, 1, 0, Q, N, 1, 0, INDXQ, 1, 0, 1.0, z, 1, 0, DLAMBDA, 1, 0, w, 1, 0, Q2, N, 0, INDX, 1, 0, INDXC, 1, 0, INDXP, 1, 0, COLTYP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaed2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaed2 = main;
} else {
	dlaed2 = tmp;
}


// EXPORTS //

module.exports = dlaed2;

// exports: { "ndarray": "dlaed2.ndarray" }
