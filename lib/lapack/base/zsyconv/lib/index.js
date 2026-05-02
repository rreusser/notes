

'use strict';

/**
* Converts a complex symmetric matrix factored by zsytrf to standard form
*
* @module @stdlib/lapack/base/zsyconv
*
*
* @example
* var zsyconv = require( '@stdlib/lapack/base/zsyconv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var E = discreteUniform( N, -10, 10, opts );
*
* zsyconv.ndarray( 'upper', '1', N, A, N, 1, 0, IPIV, 1, 0, E, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsyconv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsyconv = main;
} else {
	zsyconv = tmp;
}


// EXPORTS //

module.exports = zsyconv;

// exports: { "ndarray": "zsyconv.ndarray" }
