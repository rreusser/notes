
'use strict';

/**
* Estimate the reciprocal condition number of a Hermitian indefinite matrix.
*
* @module @stdlib/lapack/base/zhecon
*
*
* @example
* var zhecon = require( '@stdlib/lapack/base/zhecon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zhecon.ndarray( 'upper', N, A, N, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhecon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhecon = main;
} else {
	zhecon = tmp;
}


// EXPORTS //

module.exports = zhecon;

// exports: { "ndarray": "zhecon.ndarray" }
