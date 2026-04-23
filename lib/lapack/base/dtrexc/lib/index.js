
'use strict';

/**
* Reorders the real Schur factorization by an orthogonal similarity transformation.
*
* @module @stdlib/lapack/base/dtrexc
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

var dtrexc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrexc = main;
} else {
	dtrexc = tmp;
}


// EXPORTS //

module.exports = dtrexc;

// exports: { "ndarray": "dtrexc.ndarray" }
