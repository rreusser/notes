
'use strict';

/**
* Permute columns of a matrix.
*
* @module @stdlib/lapack/base/dlapmt
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

var dlapmt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlapmt = main;
} else {
	dlapmt = tmp;
}


// EXPORTS //

module.exports = dlapmt;

// exports: { "ndarray": "dlapmt.ndarray" }
