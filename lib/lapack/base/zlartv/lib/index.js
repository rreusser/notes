

'use strict';

/**
* Apply a vector of complex plane rotations with real cosines to two complex vectors.
*
* @module @stdlib/lapack/base/zlartv
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

var zlartv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlartv = main;
} else {
	zlartv = tmp;
}


// EXPORTS //

module.exports = zlartv;

// exports: { "ndarray": "zlartv.ndarray" }
