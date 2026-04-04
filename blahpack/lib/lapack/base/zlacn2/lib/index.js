

'use strict';

/**
* Estimate 1-norm of a square matrix using reverse communication
*
* @module @stdlib/lapack/base/zlacn2
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

var zlacn2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlacn2 = main;
} else {
	zlacn2 = tmp;
}


// EXPORTS //

module.exports = zlacn2;

// exports: { "ndarray": "zlacn2.ndarray" }
