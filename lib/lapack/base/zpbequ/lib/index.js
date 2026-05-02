

'use strict';

/**
* Compute row and column scalings to equilibrate a complex Hermitian positive definite band matrix.
*
* @module @stdlib/lapack/base/zpbequ
*
*
* @example
* var zpbequ = require( '@stdlib/lapack/base/zpbequ' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* zpbequ.ndarray( 'upper', N, 1, AB, N, 1, 0, s, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpbequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpbequ = main;
} else {
	zpbequ = tmp;
}


// EXPORTS //

module.exports = zpbequ;

// exports: { "ndarray": "zpbequ.ndarray" }
