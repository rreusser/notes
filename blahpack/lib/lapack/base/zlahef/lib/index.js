

'use strict';

/**
* Complex Hermitian indefinite panel factorization (blocked Bunch-Kaufman)
*
* @module @stdlib/lapack/base/zlahef
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

var zlahef;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlahef = main;
} else {
	zlahef = tmp;
}


// EXPORTS //

module.exports = zlahef;

// exports: { "ndarray": "zlahef.ndarray" }
