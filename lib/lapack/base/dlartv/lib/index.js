
'use strict';

/**
* Apply a vector of real plane rotations to two vectors.
*
* @module @stdlib/lapack/base/dlartv
*
*
* @example
* var dlartv = require( '@stdlib/lapack/base/dlartv' );
*
* var N = 3;
* var x = discreteUniform( N, -10, 10, opts );
* var y = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* dlartv.ndarray( N, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlartv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlartv = main;
} else {
	dlartv = tmp;
}


// EXPORTS //

module.exports = dlartv;

// exports: { "ndarray": "dlartv.ndarray" }
