
'use strict';

/**
* Solves a symmetric indefinite system using Bunch-Kaufman factorization with BLAS-3.
*
* @module @stdlib/lapack/base/dsytrs2
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

var dsytrs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsytrs2 = main;
} else {
	dsytrs2 = tmp;
}


// EXPORTS //

module.exports = dsytrs2;

// exports: { "ndarray": "dsytrs2.ndarray" }
