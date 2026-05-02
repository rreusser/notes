

'use strict';

/**
* Apply a vector of complex plane rotations with real cosines to two complex vectors.
*
* @module @stdlib/lapack/base/zlartv
*
*
* @example
* var zlartv = require( '@stdlib/lapack/base/zlartv' );
*
* var N = 3;
* var x = discreteUniform( N, -10, 10, opts );
* var y = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* zlartv.ndarray( N, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlartv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlartv = main;
} else {
	zlartv = tmp;
}


// EXPORTS //

module.exports = zlartv;

// exports: { "ndarray": "zlartv.ndarray" }
