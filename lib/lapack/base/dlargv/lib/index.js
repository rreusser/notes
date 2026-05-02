
'use strict';

/**
* Generate a vector of real plane rotations.
*
* @module @stdlib/lapack/base/dlargv
*
*
* @example
* var dlargv = require( '@stdlib/lapack/base/dlargv' );
*
* var N = 3;
* var x = discreteUniform( N, -10, 10, opts );
* var y = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
*
* dlargv.ndarray( N, x, 1, 0, y, 1, 0, c, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlargv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlargv = main;
} else {
	dlargv = tmp;
}


// EXPORTS //

module.exports = dlargv;

// exports: { "ndarray": "dlargv.ndarray" }
