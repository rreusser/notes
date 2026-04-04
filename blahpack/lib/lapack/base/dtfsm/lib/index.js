
'use strict';

/**
* Solves a matrix equation with a triangular matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/dtfsm
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtfsm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtfsm = main;
} else {
	dtfsm = tmp;
}


// EXPORTS //

module.exports = dtfsm;

// exports: { "ndarray": "dtfsm.ndarray" }
