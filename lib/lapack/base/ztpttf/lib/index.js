

'use strict';

/**
* Copy a triangular matrix from standard packed format (TP) to rectangular full packed format (RFP), complex version.
*
* @module @stdlib/lapack/base/ztpttf
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

var ztpttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpttf = main;
} else {
	ztpttf = tmp;
}


// EXPORTS //

module.exports = ztpttf;

// exports: { "ndarray": "ztpttf.ndarray" }
