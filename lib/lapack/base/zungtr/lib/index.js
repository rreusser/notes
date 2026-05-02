

'use strict';

/**
* Generate unitary matrix Q from zhetrd
*
* @module @stdlib/lapack/base/zungtr
*
*
* @example
* var zungtr = require( '@stdlib/lapack/base/zungtr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zungtr.ndarray( 'upper', N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zungtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zungtr = main;
} else {
	zungtr = tmp;
}


// EXPORTS //

module.exports = zungtr;

// exports: { "ndarray": "zungtr.ndarray" }
