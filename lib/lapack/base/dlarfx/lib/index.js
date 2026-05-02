
'use strict';

/**
* Applies an elementary reflector to a general matrix with unrolled loops.
*
* @module @stdlib/lapack/base/dlarfx
*
*
* @example
* var dlarfx = require( '@stdlib/lapack/base/dlarfx' );
*
* var N = 3;
* var C = discreteUniform( N * N, -10, 10, opts );
* var v = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlarfx.ndarray( 'left', N, N, v, 1, 0, 1.0, C, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarfx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarfx = main;
} else {
	dlarfx = tmp;
}


// EXPORTS //

module.exports = dlarfx;

// exports: { "ndarray": "dlarfx.ndarray" }
