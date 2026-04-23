
'use strict';

/**
* Reduces a symmetric definite generalized eigenproblem to standard form (unblocked).
*
* @module @stdlib/lapack/base/dsygs2
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

var dsygs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygs2 = main;
} else {
	dsygs2 = tmp;
}


// EXPORTS //

module.exports = dsygs2;

// exports: { "ndarray": "dsygs2.ndarray" }
