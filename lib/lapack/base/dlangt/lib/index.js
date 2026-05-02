
'use strict';

/**
* Compute the norm of a general tridiagonal matrix.
*
* @module @stdlib/lapack/base/dlangt
*
*
* @example
* var dlangt = require( '@stdlib/lapack/base/dlangt' );
*
* var N = 3;
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N, -10, 10, opts );
*
* dlangt.ndarray( '1', N, DL, 1, 0, d, 1, 0, DU, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlangt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlangt = main;
} else {
	dlangt = tmp;
}


// EXPORTS //

module.exports = dlangt;

// exports: { "ndarray": "dlangt.ndarray" }
