
'use strict';

/**
* Sets a scalar multiple of the first column of H - shift product.
*
* @module @stdlib/lapack/base/dlaqr1
*
*
* @example
* var dlaqr1 = require( '@stdlib/lapack/base/dlaqr1' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var v = discreteUniform( N, -10, 10, opts );
*
* dlaqr1.ndarray( N, H, N, 1, 0, 1, 1, 1, 1, v, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqr1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr1 = main;
} else {
	dlaqr1 = tmp;
}


// EXPORTS //

module.exports = dlaqr1;

// exports: { "ndarray": "dlaqr1.ndarray" }
