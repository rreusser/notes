
'use strict';

/**
* Provides error bounds for solution of a triangular system.
*
* @module @stdlib/lapack/base/dtrrfs
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

var dtrrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrrfs = main;
} else {
	dtrrfs = tmp;
}


// EXPORTS //

module.exports = dtrrfs;

// exports: { "ndarray": "dtrrfs.ndarray" }
