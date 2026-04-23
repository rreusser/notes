

'use strict';

/**
* Applies a complex unitary matrix Q from a QL factorization to a matrix (blocked)
*
* @module @stdlib/lapack/base/zunmql
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

var zunmql;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmql = main;
} else {
	zunmql = tmp;
}


// EXPORTS //

module.exports = zunmql;

// exports: { "ndarray": "zunmql.ndarray" }
