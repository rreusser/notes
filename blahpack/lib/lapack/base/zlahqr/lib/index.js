

'use strict';

/**
* Compute eigenvalues and Schur form of upper Hessenberg matrix
*
* @module @stdlib/lapack/base/zlahqr
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

var zlahqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlahqr = main;
} else {
	zlahqr = tmp;
}


// EXPORTS //

module.exports = zlahqr;

// exports: { "ndarray": "zlahqr.ndarray" }
