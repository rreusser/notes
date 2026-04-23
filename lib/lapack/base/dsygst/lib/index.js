
'use strict';

/**
* Reduces a symmetric definite generalized eigenproblem to standard form (blocked).
*
* @module @stdlib/lapack/base/dsygst
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

var dsygst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygst = main;
} else {
	dsygst = tmp;
}


// EXPORTS //

module.exports = dsygst;

// exports: { "ndarray": "dsygst.ndarray" }
