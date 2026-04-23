

'use strict';

/**
* Complex Hermitian indefinite solve using factorization from ZHETRF
*
* @module @stdlib/lapack/base/zhetrs2
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

var zhetrs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetrs2 = main;
} else {
	zhetrs2 = tmp;
}


// EXPORTS //

module.exports = zhetrs2;

// exports: { "ndarray": "zhetrs2.ndarray" }
