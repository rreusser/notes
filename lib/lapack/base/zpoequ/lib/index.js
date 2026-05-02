

'use strict';

/**
* Compute row/column scaling for Hermitian positive definite matrix
*
* @module @stdlib/lapack/base/zpoequ
*
*
* @example
* var zpoequ = require( '@stdlib/lapack/base/zpoequ' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* zpoequ.ndarray( N, A, N, 1, 0, s, 1, 0, 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpoequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpoequ = main;
} else {
	zpoequ = tmp;
}


// EXPORTS //

module.exports = zpoequ;

// exports: { "ndarray": "zpoequ.ndarray" }
