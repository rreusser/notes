
'use strict';

/**
* Complex blocked RQ factorization.
*
* @module @stdlib/lapack/base/zgerqf
*
*
* @example
* var zgerqf = require( '@stdlib/lapack/base/zgerqf' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zgerqf.ndarray( N, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgerqf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgerqf = main;
} else {
	zgerqf = tmp;
}


// EXPORTS //

module.exports = zgerqf;

// exports: { "ndarray": "zgerqf.ndarray" }
