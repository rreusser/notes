
'use strict';

/**
* Compute row and column scalings to equilibrate a symmetric positive definite matrix in packed storage.
*
* @module @stdlib/lapack/base/dppequ
*
*
* @example
* var dppequ = require( '@stdlib/lapack/base/dppequ' );
*
* var N = 3;
* var AP = discreteUniform( N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
*
* dppequ.ndarray( 'upper', N, AP, 1, 0, s, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dppequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dppequ = main;
} else {
	dppequ = tmp;
}


// EXPORTS //

module.exports = dppequ;

// exports: { "ndarray": "dppequ.ndarray" }
