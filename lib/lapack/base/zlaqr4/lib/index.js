

'use strict';

/**
* Complex multishift QR with aggressive early deflation (non-recursive)
*
* @module @stdlib/lapack/base/zlaqr4
*
*
* @example
* var zlaqr4 = require( '@stdlib/lapack/base/zlaqr4' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zlaqr4.ndarray( 1, 1, N, N, N, H, N, 1, 0, w, 1, 0, 1, 1, Z, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqr4;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr4 = main;
} else {
	zlaqr4 = tmp;
}


// EXPORTS //

module.exports = zlaqr4;

// exports: { "ndarray": "zlaqr4.ndarray" }
