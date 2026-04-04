
'use strict';

/**
* Solves a complex system A * X = B where A is Hermitian positive definite in packed storage, with equilibration, condition estimation, and error bounds.
*
* @module @stdlib/lapack/base/zppsvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zppsvx = require( '@stdlib/lapack/base/zppsvx' );
*
* var AP = new Complex128Array( new Float64Array( [ 10, 0, 3, -1, 8, 0, 1, 2, 2, -1, 6, 0 ] ) );
* var AFP = new Complex128Array( 6 );
* var S = new Float64Array( 3 );
* var equed = [ 'none' ];
* var B = new Complex128Array( new Float64Array( [ 1, 1, 2, -1, 3, 0.5 ] ) );
* var X = new Complex128Array( 3 );
* var rcond = new Float64Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = zppsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zppsvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zppsvx = main;
} else {
	zppsvx = tmp;
}


// EXPORTS //

module.exports = zppsvx;

// exports: { "ndarray": "zppsvx.ndarray" }
