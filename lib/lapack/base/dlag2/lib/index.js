
'use strict';

/**
* Compute the eigenvalues of a 2-by-2 generalized eigenvalue problem.
*
* @module @stdlib/lapack/base/dlag2
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

var dlag2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlag2 = main;
} else {
	dlag2 = tmp;
}


// EXPORTS //

module.exports = dlag2;

// exports: { "ndarray": "dlag2.ndarray" }
