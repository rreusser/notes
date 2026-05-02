

'use strict';

/**
* Compute eigenvalues and Schur form of upper Hessenberg matrix
*
* @module @stdlib/lapack/base/zlahqr
*
*
* @example
* var zlahqr = require( '@stdlib/lapack/base/zlahqr' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var W = discreteUniform( N, -10, 10, opts );
*
* zlahqr.ndarray( 1, 1, N, N, N, H, N, 1, 0, W, 1, 0, 1, 1, Z, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlahqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlahqr = main;
} else {
	zlahqr = tmp;
}


// EXPORTS //

module.exports = zlahqr;

// exports: { "ndarray": "zlahqr.ndarray" }
