
'use strict';

/**
* Complex unblocked RQ factorization.
*
* @module @stdlib/lapack/base/zgerq2
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

var zgerq2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgerq2 = main;
} else {
	zgerq2 = tmp;
}


// EXPORTS //

module.exports = zgerq2;

// exports: { "ndarray": "zgerq2.ndarray" }
