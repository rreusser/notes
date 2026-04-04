
'use strict';

/**
* Reorders the Schur factorization and computes condition numbers.
*
* @module @stdlib/lapack/base/dtrsen
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

var dtrsen;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrsen = main;
} else {
	dtrsen = tmp;
}


// EXPORTS //

module.exports = dtrsen;

// exports: { "ndarray": "dtrsen.ndarray" }
