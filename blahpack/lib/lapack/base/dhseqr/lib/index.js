
'use strict';

/**
* Computes eigenvalues and Schur decomposition of an upper Hessenberg matrix.
*
* @module @stdlib/lapack/base/dhseqr
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

var dhseqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dhseqr = main;
} else {
	dhseqr = tmp;
}


// EXPORTS //

module.exports = dhseqr;

// exports: { "ndarray": "dhseqr.ndarray" }
