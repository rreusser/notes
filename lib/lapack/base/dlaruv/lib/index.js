
'use strict';

/**
* Generates a vector of random numbers from a uniform distribution.
*
* @module @stdlib/lapack/base/dlaruv
*
*
* @example
* var dlaruv = require( '@stdlib/lapack/base/dlaruv' );
*
* var N = 3;
* var iseed = discreteUniform( N, -10, 10, opts );
* var x = discreteUniform( N, -10, 10, opts );
*
* dlaruv.ndarray( iseed, 1, 0, N, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaruv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaruv = main;
} else {
	dlaruv = tmp;
}


// EXPORTS //

module.exports = dlaruv;

// exports: { "ndarray": "dlaruv.ndarray" }
