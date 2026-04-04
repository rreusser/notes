

'use strict';

/**
* Complex Hermitian indefinite factorization (unblocked Bunch-Kaufman)
*
* @module @stdlib/lapack/base/zhetf2
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

var zhetf2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetf2 = main;
} else {
	zhetf2 = tmp;
}


// EXPORTS //

module.exports = zhetf2;

// exports: { "ndarray": "zhetf2.ndarray" }
