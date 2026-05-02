
'use strict';

/**
* Generates a vector of random numbers from a specified distribution.
*
* @module @stdlib/lapack/base/dlarnv
*
*
* @example
* var dlarnv = require( '@stdlib/lapack/base/dlarnv' );
*
* var N = 3;
* var iseed = discreteUniform( N, -10, 10, opts );
*
* dlarnv.ndarray( 1, iseed, 1, 0, N, 1, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarnv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarnv = main;
} else {
	dlarnv = tmp;
}


// EXPORTS //

module.exports = dlarnv;

// exports: { "ndarray": "dlarnv.ndarray" }
