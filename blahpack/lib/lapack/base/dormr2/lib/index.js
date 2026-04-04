
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from RQ factorization (unblocked).
*
* @module @stdlib/lapack/base/dormr2
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

var dormr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormr2 = main;
} else {
	dormr2 = tmp;
}


// EXPORTS //

module.exports = dormr2;

// exports: { "ndarray": "dormr2.ndarray" }
