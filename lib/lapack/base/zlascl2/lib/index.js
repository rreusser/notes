

'use strict';

/**
* Perform diagonal scaling on a complex matrix.
*
* @module @stdlib/lapack/base/zlascl2
*
*
* @example
* var zlascl2 = require( '@stdlib/lapack/base/zlascl2' );
*
* var N = 3;
* var X = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
*
* zlascl2.ndarray( N, N, d, 1, 0, X, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlascl2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlascl2 = main;
} else {
	zlascl2 = tmp;
}


// EXPORTS //

module.exports = zlascl2;

// exports: { "ndarray": "zlascl2.ndarray" }
