
'use strict';

/**
* Apply a vector of real plane rotations from both sides to a sequence of 2-by-2 symmetric matrices.
*
* @module @stdlib/lapack/base/dlar2v
*
*
* @example
* var dlar2v = require( '@stdlib/lapack/base/dlar2v' );
*
* var N = 3;
* var x = discreteUniform( N, -10, 10, opts );
* var y = discreteUniform( N, -10, 10, opts );
* var z = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* dlar2v.ndarray( N, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlar2v;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlar2v = main;
} else {
	dlar2v = tmp;
}


// EXPORTS //

module.exports = dlar2v;

// exports: { "ndarray": "dlar2v.ndarray" }
