

'use strict';

/**
* Computes row and column scalings for equilibrating a general matrix
*
* @module @stdlib/lapack/base/dgeequ
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

var dgeequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeequ = main;
} else {
	dgeequ = tmp;
}


// EXPORTS //

module.exports = dgeequ;

// exports: { "ndarray": "dgeequ.ndarray" }
