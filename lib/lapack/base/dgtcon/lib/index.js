
'use strict';

/**
* Estimate the reciprocal of the condition number of a real general tridiagonal matrix.
*
* @module @stdlib/lapack/base/dgtcon
*
*
* @example
* var dgtcon = require( '@stdlib/lapack/base/dgtcon' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dgtcon.ndarray( '1', N, DL, 1, 0, d, 1, 0, DU, 1, 0, 1, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgtcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtcon = main;
} else {
	dgtcon = tmp;
}


// EXPORTS //

module.exports = dgtcon;

// exports: { "ndarray": "dgtcon.ndarray" }
