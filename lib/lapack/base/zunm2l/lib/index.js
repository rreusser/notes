

'use strict';

/**
* Applies a complex unitary matrix Q from a QL factorization to a matrix (unblocked)
*
* @module @stdlib/lapack/base/zunm2l
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

var zunm2l;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunm2l = main;
} else {
	zunm2l = tmp;
}


// EXPORTS //

module.exports = zunm2l;

// exports: { "ndarray": "zunm2l.ndarray" }
