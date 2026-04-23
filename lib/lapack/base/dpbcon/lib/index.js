
'use strict';

/**
* Estimates the reciprocal condition number of a positive definite banded matrix.
*
* @module @stdlib/lapack/base/dpbcon
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

var dpbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpbcon = main;
} else {
	dpbcon = tmp;
}


// EXPORTS //

module.exports = dpbcon;

// exports: { "ndarray": "dpbcon.ndarray" }
