
'use strict';

/**
* Refine eigenvalue approximations using bisection given initial intervals.
*
* @module @stdlib/lapack/base/dlarrj
*
*
* @example
* var dlarrj = require( '@stdlib/lapack/base/dlarrj' );
*
* var N = 3;
* var d = discreteUniform( N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dlarrj.ndarray( N, d, 1, 0, 1, 1, 0, 1, 1, 1, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, 1.0, 1 );
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
