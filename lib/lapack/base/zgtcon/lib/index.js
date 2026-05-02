
'use strict';

/**
* Estimate reciprocal condition number of complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zgtcon
*
*
* @example
* var zgtcon = require( '@stdlib/lapack/base/zgtcon' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zgtcon.ndarray( '1', N, DL, 1, 0, d, 1, 0, DU, 1, 0, 1, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgtcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtcon = main;
} else {
	zgtcon = tmp;
}


// EXPORTS //

module.exports = zgtcon;

// exports: { "ndarray": "zgtcon.ndarray" }
