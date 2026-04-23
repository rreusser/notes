'use strict';

/**
* Estimates the reciprocal condition number of a real symmetric packed matrix.
*
* @module @stdlib/lapack/base/dspcon
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dspcon = require( '@stdlib/lapack/base/dspcon' );
*
* // Factored 3x3 identity in upper packed storage (already factored):
* var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
* var IPIV = new Int32Array( [ 0, 1, 2 ] );
* var rcond = new Float64Array( 1 );
* var WORK = new Float64Array( 6 );
* var IWORK = new Int32Array( 3 );
*
* var info = dspcon.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, 1.0, rcond, WORK, 1, 0, IWORK, 1, 0 );
* // returns 0
* // rcond[ 0 ] => 1.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspcon = main;
} else {
	dspcon = tmp;
}


// EXPORTS //

module.exports = dspcon;

// exports: { "ndarray": "dspcon.ndarray" }
