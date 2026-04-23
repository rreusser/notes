
'use strict';

/**
* Refine eigenvalue approximations using bisection given initial intervals.
*
* @module @stdlib/lapack/base/dlarrj
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

var dlarrj;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarrj = main;
} else {
	dlarrj = tmp;
}


// EXPORTS //

module.exports = dlarrj;

// exports: { "ndarray": "dlarrj.ndarray" }
