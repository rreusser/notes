
'use strict';

/**
* Perform diagonal scaling on a matrix.
*
* @module @stdlib/lapack/base/dlascl2
*
*
* @example
* var dlascl2 = require( '@stdlib/lapack/base/dlascl2' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
*
* dlascl2.ndarray( N, N, d, 1, 0, X, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlascl2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlascl2 = main;
} else {
	dlascl2 = tmp;
}


// EXPORTS //

module.exports = dlascl2;

// exports: { "ndarray": "dlascl2.ndarray" }
