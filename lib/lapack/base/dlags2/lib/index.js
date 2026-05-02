
'use strict';

/**
* Computes 2-by-2 orthogonal matrices U, V, Q for simultaneous upper/lower triangularization.
*
* @module @stdlib/lapack/base/dlags2
*
*
* @example
* var dlags2 = require( '@stdlib/lapack/base/dlags2' );
*
* dlags2.ndarray( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlags2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlags2 = main;
} else {
	dlags2 = tmp;
}


// EXPORTS //

module.exports = dlags2;

// exports: { "ndarray": "dlags2.ndarray" }
