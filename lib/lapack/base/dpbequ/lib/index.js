
'use strict';

/**
* Compute row and column scalings to equilibrate a symmetric positive definite band matrix.
*
* @module @stdlib/lapack/base/dpbequ
*
*
* @example
* var dpbequ = require( '@stdlib/lapack/base/dpbequ' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* dpbequ.ndarray( 'upper', N, 1, AB, N, 1, 0, s, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpbequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpbequ = main;
} else {
	dpbequ = tmp;
}


// EXPORTS //

module.exports = dpbequ;

// exports: { "ndarray": "dpbequ.ndarray" }
