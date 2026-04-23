

'use strict';

/**
* Multiplies a general matrix by the unitary matrix Q from an RQ factorization (blocked)
*
* @module @stdlib/lapack/base/zunmrq
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

var zunmrq;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmrq = main;
} else {
	zunmrq = tmp;
}


// EXPORTS //

module.exports = zunmrq;

// exports: { "ndarray": "zunmrq.ndarray" }
