
'use strict';

/**
* Computes a generalized RQ factorization of an M-by-N complex matrix A and a P-by-N complex matrix B.
*
* @module @stdlib/lapack/base/zggrqf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zggrqf = require( '@stdlib/lapack/base/zggrqf' );
*
* var A = new Complex128Array( [ 5, 2 ] );
* var TAUA = new Complex128Array( 1 );
* var B = new Complex128Array( [ 3, -1 ] );
* var TAUB = new Complex128Array( 1 );
* var WORK = new Complex128Array( 64 );
*
* var info = zggrqf.ndarray( 1, 1, 1, A, 1, 1, 0, TAUA, 1, 0, B, 1, 1, 0, TAUB, 1, 0, WORK, 1, 0, 64 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zggrqf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zggrqf = main;
} else {
	zggrqf = tmp;
}


// EXPORTS //

module.exports = zggrqf;

// exports: { "ndarray": "zggrqf.ndarray" }
