
'use strict';

/**
* Solves a real system A * X = B where A is general band, with equilibration, condition estimation, and error bounds.
*
* @module @stdlib/lapack/base/dgbsvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dgbsvx = require( '@stdlib/lapack/base/dgbsvx' );
*
* var AB = new Float64Array( [ 0, 1, 1, 4, 3, 2, 1, 1, 0 ] );
* var AFB = new Float64Array( 12 );
* var IPIV = new Int32Array( 3 );
* var r = new Float64Array( 3 );
* var c = new Float64Array( 3 );
* var B = new Float64Array( [ 5, 5, 3 ] );
* var X = new Float64Array( 3 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var out = dgbsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, 1, 1, AB, 1, 3, 0, AFB, 1, 4, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // out.info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgbsvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgbsvx = main;
} else {
	dgbsvx = tmp;
}


// EXPORTS //

module.exports = dgbsvx;

// exports: { "ndarray": "dgbsvx.ndarray" }
