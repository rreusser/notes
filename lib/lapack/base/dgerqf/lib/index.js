
'use strict';

/**
* Compute the RQ factorization of a real matrix (blocked).
*
* @module @stdlib/lapack/base/dgerqf
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

var dgerqf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgerqf = main;
} else {
	dgerqf = tmp;
}


// EXPORTS //

module.exports = dgerqf;

// exports: { "ndarray": "dgerqf.ndarray" }
