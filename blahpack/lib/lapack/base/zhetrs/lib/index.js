

'use strict';

/**
* Solve a system of linear equations A*X = B with a Hermitian indefinite matrix using Bunch-Kaufman factorization
*
* @module @stdlib/lapack/base/zhetrs
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

var zhetrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetrs = main;
} else {
	zhetrs = tmp;
}


// EXPORTS //

module.exports = zhetrs;

// exports: { "ndarray": "zhetrs.ndarray" }
