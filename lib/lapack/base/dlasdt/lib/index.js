
'use strict';

/**
* Create a tree of subproblems for bidiagonal divide and conquer.
*
* @module @stdlib/lapack/base/dlasdt
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

var dlasdt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasdt = main;
} else {
	dlasdt = tmp;
}


// EXPORTS //

module.exports = dlasdt;

// exports: { "ndarray": "dlasdt.ndarray" }
