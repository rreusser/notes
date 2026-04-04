

'use strict';

/**
* Equilibrate a complex general matrix using row and column scalings
*
* @module @stdlib/lapack/base/zlaqge
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

var zlaqge;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqge = main;
} else {
	zlaqge = tmp;
}


// EXPORTS //

module.exports = zlaqge;

// exports: { "ndarray": "zlaqge.ndarray" }
