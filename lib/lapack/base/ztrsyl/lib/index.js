

'use strict';

/**
* Solve complex Sylvester matrix equation
*
* @module @stdlib/lapack/base/ztrsyl
*
*
* @example
* var ztrsyl = require( '@stdlib/lapack/base/ztrsyl' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
*
* ztrsyl.ndarray( 1, 1, 1, N, N, A, N, 1, 0, B, N, 1, 0, C, N, 1, 0, 1.0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrsyl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrsyl = main;
} else {
	ztrsyl = tmp;
}


// EXPORTS //

module.exports = ztrsyl;

// exports: { "ndarray": "ztrsyl.ndarray" }
