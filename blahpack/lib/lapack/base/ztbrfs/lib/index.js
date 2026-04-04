
'use strict';

/**
* Provides error bounds for the solution to a system with a complex triangular band matrix.
*
* @module @stdlib/lapack/base/ztbrfs
*
* @example
* var ztbrfs = require( '@stdlib/lapack/base/ztbrfs' );
* // See README for usage examples
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztbrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztbrfs = main;
} else {
	ztbrfs = tmp;
}


// EXPORTS //

module.exports = ztbrfs;

// exports: { "ndarray": "ztbrfs.ndarray" }
