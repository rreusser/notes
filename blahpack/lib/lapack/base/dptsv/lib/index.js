
'use strict';

/**
* Solves a real symmetric positive definite tridiagonal system of linear equations.
*
* @module @stdlib/lapack/base/dptsv
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

var dptsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dptsv = main;
} else {
	dptsv = tmp;
}


// EXPORTS //

module.exports = dptsv;

// exports: { "ndarray": "dptsv.ndarray" }
