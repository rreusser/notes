
'use strict';

/**
* Construct a Givens plane rotation with real cosine and complex sine.
*
* @module @stdlib/blas/base/zrotg
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

var zrotg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zrotg = main;
} else {
	zrotg = tmp;
}


// EXPORTS //

module.exports = zrotg;

// exports: { "ndarray": "zrotg.ndarray" }
