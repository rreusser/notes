

'use strict';

/**
* Complex multi-shift QR sweep
*
* @module @stdlib/lapack/base/zlaqr5
*
*
* @example
* var zlaqr5 = require( '@stdlib/lapack/base/zlaqr5' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var V = discreteUniform( N * N, -10, 10, opts );
* var U = discreteUniform( N * N, -10, 10, opts );
* var WV = discreteUniform( N * N, -10, 10, opts );
* var WH = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* zlaqr5.ndarray( 1, 1, 1, N, 1, 1, 1, s, 1, 0, H, N, 1, 0, 1, 1, Z, N, 1, 0, V, N, 1, 0, U, N, 1, 0, 1, WV, N, 1, 0, 1, WH, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqr5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr5 = main;
} else {
	zlaqr5 = tmp;
}


// EXPORTS //

module.exports = zlaqr5;

// exports: { "ndarray": "zlaqr5.ndarray" }
