
'use strict';

/**
* Solves a complex system A * X = B where A is general band, with equilibration, condition estimation, and error bounds.
*
* @module @stdlib/lapack/base/zgbsvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zgbsvx = require( '@stdlib/lapack/base/zgbsvx' );
*
* var AB = new Complex128Array( [ 0,0, 1,0, 1,0, 4,0, 3,0, 2,0, 1,0, 1,0, 0,0 ] );
* var AFB = new Complex128Array( 4 * 3 );
* var IPIV = new Int32Array( 3 );
* var r = new Float64Array( 3 );
* var c = new Float64Array( 3 );
* var B = new Complex128Array( [ 5,0, 5,0, 3,0 ] );
* var X = new Complex128Array( 3 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var out = zgbsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, 1, 1, AB, 1, 3, 0, AFB, 1, 4, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // out.info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgbsvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbsvx = main;
} else {
	zgbsvx = tmp;
}


// EXPORTS //

module.exports = zgbsvx;

// exports: { "ndarray": "zgbsvx.ndarray" }
