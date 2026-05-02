

'use strict';

/**
* Complex aggressive early deflation (non-recursive)
*
* @module @stdlib/lapack/base/zlaqr2
*
*
* @example
* var zlaqr2 = require( '@stdlib/lapack/base/zlaqr2' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var V = discreteUniform( N * N, -10, 10, opts );
* var T = discreteUniform( N * N, -10, 10, opts );
* var WV = discreteUniform( N * N, -10, 10, opts );
* var SH = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zlaqr2.ndarray( 1, 1, N, 1, 1, 1, H, N, 1, 0, 1, 1, Z, N, 1, 0, 1, 1, SH, 1, 0, V, N, 1, 0, 1, T, N, 1, 0, 1, WV, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr2 = main;
} else {
	zlaqr2 = tmp;
}


// EXPORTS //

module.exports = zlaqr2;

// exports: { "ndarray": "zlaqr2.ndarray" }
