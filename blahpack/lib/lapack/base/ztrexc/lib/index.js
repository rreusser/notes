

'use strict';

/**
* Reorder Schur factorization of a complex matrix
*
* @module @stdlib/lapack/base/ztrexc
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

var ztrexc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrexc = main;
} else {
	ztrexc = tmp;
}


// EXPORTS //

module.exports = ztrexc;

// exports: { "ndarray": "ztrexc.ndarray" }
