
'use strict';

/**
* Solve the 2-by-2 secular equation.
*
* @module @stdlib/lapack/base/dlaed5
*
*
* @example
* var dlaed5 = require( '@stdlib/lapack/base/dlaed5' );
*
* var D = discreteUniform( N, -10, 10, opts );
* var Z = discreteUniform( N, -10, 10, opts );
* var DELTA = discreteUniform( N, -10, 10, opts );
*
* dlaed5.ndarray( 1, D, 1, 0, Z, 1, 0, DELTA, 1, 0, 1.0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaed5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaed5 = main;
} else {
	dlaed5 = tmp;
}


// EXPORTS //

module.exports = dlaed5;

// exports: { "ndarray": "dlaed5.ndarray" }
