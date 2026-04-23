

'use strict';

/**
* Applies a complex unitary matrix from zhetrd to a matrix
*
* @module @stdlib/lapack/base/zunmtr
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

var zunmtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmtr = main;
} else {
	zunmtr = tmp;
}


// EXPORTS //

module.exports = zunmtr;

// exports: { "ndarray": "zunmtr.ndarray" }
