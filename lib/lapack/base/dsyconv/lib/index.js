
'use strict';

/**
* Converts a symmetric matrix factored by dsytrf to standard L_D_L^T form and vice versa.
*
* @module @stdlib/lapack/base/dsyconv
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

var dsyconv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsyconv = main;
} else {
	dsyconv = tmp;
}


// EXPORTS //

module.exports = dsyconv;

// exports: { "ndarray": "dsyconv.ndarray" }
