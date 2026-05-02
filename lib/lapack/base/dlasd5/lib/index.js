
'use strict';

/**
* Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix.
*
* @module @stdlib/lapack/base/dlasd5
*
*
* @example
* var dlasd5 = require( '@stdlib/lapack/base/dlasd5' );
*
* var d = discreteUniform( N, -10, 10, opts );
* var z = discreteUniform( N, -10, 10, opts );
* var DELTA = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlasd5.ndarray( 1, d, 1, 0, z, 1, 0, DELTA, 1, 0, 1.0, 1, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasd5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasd5 = main;
} else {
	dlasd5 = tmp;
}


// EXPORTS //

module.exports = dlasd5;

// exports: { "ndarray": "dlasd5.ndarray" }
