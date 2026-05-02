

'use strict';

/**
* Generate unitary matrix Q from QL reflectors
*
* @module @stdlib/lapack/base/zung2l
*
*
* @example
* var zung2l = require( '@stdlib/lapack/base/zung2l' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zung2l.ndarray( N, N, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zung2l;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zung2l = main;
} else {
	zung2l = tmp;
}


// EXPORTS //

module.exports = zung2l;

// exports: { "ndarray": "zung2l.ndarray" }
