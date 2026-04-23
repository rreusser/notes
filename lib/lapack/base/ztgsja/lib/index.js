
'use strict';

/**
* Computes the generalized singular value decomposition of two complex upper triangular matrices.
*
* @module @stdlib/lapack/base/ztgsja
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var ztgsja = require( '@stdlib/lapack/base/ztgsja' );
*
* var A = new Complex128Array( 4 );
* var B = new Complex128Array( 4 );
* var U = new Complex128Array( 4 );
* var V = new Complex128Array( 4 );
* var Q = new Complex128Array( 4 );
* var WORK = new Complex128Array( 4 );
* var ALPHA = new Float64Array( 2 );
* var BETA = new Float64Array( 2 );
* var ncycle = new Int32Array( 1 );
*
* var info = ztgsja.ndarray( 'initialize', 'initialize', 'initialize', 2, 2, 2, 0, 2, A, 1, 2, 0, B, 1, 2, 0, 1e-14, 1e-14, ALPHA, 1, 0, BETA, 1, 0, U, 1, 2, 0, V, 1, 2, 0, Q, 1, 2, 0, WORK, 1, 0, ncycle );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztgsja;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztgsja = main;
} else {
	ztgsja = tmp;
}


// EXPORTS //

module.exports = ztgsja;

// exports: { "ndarray": "ztgsja.ndarray" }
