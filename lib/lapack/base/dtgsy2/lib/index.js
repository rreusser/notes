
'use strict';

/**
* Solves the generalized Sylvester equation (unblocked).
*
* @module @stdlib/lapack/base/dtgsy2
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

var dtgsy2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtgsy2 = main;
} else {
	dtgsy2 = tmp;
}


// EXPORTS //

module.exports = dtgsy2;

// exports: { "ndarray": "dtgsy2.ndarray" }
