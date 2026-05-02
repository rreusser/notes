

'use strict';

/**
* Complex symmetric matrix-vector multiply
*
* @module @stdlib/lapack/base/zsymv
*
*
* @example
* var zsymv = require( '@stdlib/lapack/base/zsymv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var x = discreteUniform( N, -10, 10, opts );
* var y = discreteUniform( N, -10, 10, opts );
*
* zsymv.ndarray( 'upper', N, 1.0, A, N, 1, 0, x, 1, 0, 1.0, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsymv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsymv = main;
} else {
	zsymv = tmp;
}


// EXPORTS //

module.exports = zsymv;

// exports: { "ndarray": "zsymv.ndarray" }
