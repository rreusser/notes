

'use strict';

/**
* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (TF)
*
* @module @stdlib/lapack/base/ztrttf
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

var ztrttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrttf = main;
} else {
	ztrttf = tmp;
}


// EXPORTS //

module.exports = ztrttf;

// exports: { "ndarray": "ztrttf.ndarray" }
