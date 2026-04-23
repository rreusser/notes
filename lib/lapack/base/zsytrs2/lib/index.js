

'use strict';

/**
* Solves a complex symmetric indefinite system using Bunch-Kaufman factorization with BLAS-3
*
* @module @stdlib/lapack/base/zsytrs2
*
* @example
* // TODO: Add example
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytrs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytrs2 = main;
} else {
	zsytrs2 = tmp;
}


// EXPORTS //

module.exports = zsytrs2;

// exports: { "ndarray": "zsytrs2.ndarray" }
