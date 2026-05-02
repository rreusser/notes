
'use strict';

/**
* Complex unblocked RQ factorization.
*
* @module @stdlib/lapack/base/zgerq2
*
*
* @example
* var zgerq2 = require( '@stdlib/lapack/base/zgerq2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zgerq2.ndarray( N, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgerq2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgerq2 = main;
} else {
	zgerq2 = tmp;
}


// EXPORTS //

module.exports = zgerq2;

// exports: { "ndarray": "zgerq2.ndarray" }
