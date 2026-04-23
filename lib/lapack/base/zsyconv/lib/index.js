

'use strict';

/**
* Converts a complex symmetric matrix factored by zsytrf to standard form
*
* @module @stdlib/lapack/base/zsyconv
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

var zsyconv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsyconv = main;
} else {
	zsyconv = tmp;
}


// EXPORTS //

module.exports = zsyconv;

// exports: { "ndarray": "zsyconv.ndarray" }
