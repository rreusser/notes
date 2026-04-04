
'use strict';

/**
* Estimates the reciprocal condition number of a complex Hermitian positive definite matrix in packed storage.
*
* @module @stdlib/lapack/base/zppcon
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zppcon = require( '@stdlib/lapack/base/zppcon' );
*
* var AP = new Complex128Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0 ] );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
* var rcond = new Float64Array( 1 );
*
* var info = zppcon.ndarray( 'upper', 3, AP, 1, 0, 1.0, rcond, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zppcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zppcon = main;
} else {
	zppcon = tmp;
}


// EXPORTS //

module.exports = zppcon;

// exports: { "ndarray": "zppcon.ndarray" }
