

'use strict';

/**
* Estimate the reciprocal of the condition number of a complex symmetric indefinite matrix
*
* @module @stdlib/lapack/base/zsycon
*
*
* @example
* var zsycon = require( '@stdlib/lapack/base/zsycon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zsycon.ndarray( 'upper', N, A, N, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsycon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsycon = main;
} else {
	zsycon = tmp;
}


// EXPORTS //

module.exports = zsycon;

// exports: { "ndarray": "zsycon.ndarray" }
