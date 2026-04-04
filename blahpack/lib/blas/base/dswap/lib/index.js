
'use strict';

/**
* Interchange two vectors.
*
* @module @stdlib/blas/base/dswap
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

var dswap;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dswap = main;
} else {
	dswap = tmp;
}


// EXPORTS //

module.exports = dswap;

// exports: { "ndarray": "dswap.ndarray" }
