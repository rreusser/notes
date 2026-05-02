
'use strict';

/**
* Solves a tridiagonal system factored by dlagtf.
*
* @module @stdlib/lapack/base/dlagts
*
*
* @example
* var dlagts = require( '@stdlib/lapack/base/dlagts' );
*
* var N = 3;
* var a = discreteUniform( N, -10, 10, opts );
* var b = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var IN = discreteUniform( N, -10, 10, opts );
* var y = discreteUniform( N, -10, 10, opts );
*
* dlagts.ndarray( 'both', N, a, 1, 0, b, 1, 0, c, 1, 0, d, 1, 0, IN, 1, 0, y, 1, 0, 1.0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlagts;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlagts = main;
} else {
	dlagts = tmp;
}


// EXPORTS //

module.exports = dlagts;

// exports: { "ndarray": "dlagts.ndarray" }
