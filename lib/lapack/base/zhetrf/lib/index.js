

'use strict';

/**
* Complex Hermitian indefinite factorization (blocked Bunch-Kaufman)
*
* @module @stdlib/lapack/base/zhetrf
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

var zhetrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetrf = main;
} else {
	zhetrf = tmp;
}


// EXPORTS //

module.exports = zhetrf;

// exports: { "ndarray": "zhetrf.ndarray" }
