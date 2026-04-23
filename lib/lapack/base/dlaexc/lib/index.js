
'use strict';

/**
* Swaps adjacent diagonal blocks of a real upper quasi-triangular matrix.
*
* @module @stdlib/lapack/base/dlaexc
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

var dlaexc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaexc = main;
} else {
	dlaexc = tmp;
}


// EXPORTS //

module.exports = dlaexc;

// exports: { "ndarray": "dlaexc.ndarray" }
