
'use strict';

/**
* Estimates the reciprocal condition number of a symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/dsycon
*
*
* @example
* var dsycon = require( '@stdlib/lapack/base/dsycon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dsycon.ndarray( 'upper', N, A, N, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsycon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsycon = main;
} else {
	dsycon = tmp;
}


// EXPORTS //

module.exports = dsycon;

// exports: { "ndarray": "dsycon.ndarray" }
