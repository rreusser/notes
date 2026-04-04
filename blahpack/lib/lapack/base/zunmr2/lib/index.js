

'use strict';

/**
* Multiplies a general matrix by the unitary matrix Q from an RQ factorization (unblocked)
*
* @module @stdlib/lapack/base/zunmr2
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

var zunmr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmr2 = main;
} else {
	zunmr2 = tmp;
}


// EXPORTS //

module.exports = zunmr2;

// exports: { "ndarray": "zunmr2.ndarray" }
