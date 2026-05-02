
'use strict';

/**
* Compute row/column scalings for equilibrating a symmetric positive definite matrix.
*
* @module @stdlib/lapack/base/dpoequ
*
*
* @example
* var dpoequ = require( '@stdlib/lapack/base/dpoequ' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* dpoequ.ndarray( N, A, N, 1, 0, s, 1, 0, 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpoequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpoequ = main;
} else {
	dpoequ = tmp;
}


// EXPORTS //

module.exports = dpoequ;

// exports: { "ndarray": "dpoequ.ndarray" }
