
'use strict';

/**
* Construct a Givens plane rotation.
*
* @module @stdlib/blas/base/drotg
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

var drotg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	drotg = main;
} else {
	drotg = tmp;
}


// EXPORTS //

module.exports = drotg;

// exports: { "ndarray": "drotg.ndarray" }
