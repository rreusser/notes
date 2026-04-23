

'use strict';

/**
* Estimate the reciprocal of the condition number of a complex symmetric indefinite matrix
*
* @module @stdlib/lapack/base/zsycon
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

var zsycon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsycon = main;
} else {
	zsycon = tmp;
}


// EXPORTS //

module.exports = zsycon;

// exports: { "ndarray": "zsycon.ndarray" }
