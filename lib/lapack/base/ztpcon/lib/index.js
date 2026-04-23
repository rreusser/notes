
'use strict';

/**
* Estimates the reciprocal condition number of a complex triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/ztpcon
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var ztpcon = require( '@stdlib/lapack/base/ztpcon' );
*
* var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 1.0, 3.0, 0.0, 0.5, 0.0, 1.0, -1.0, 2.0, 1.0 ] );
* var RCOND = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = ztpcon( 'one-norm', 'upper', 'non-unit', 3, AP, RCOND, WORK, RWORK );
* // info => 0
* // RCOND[ 0 ] => ~0.335
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztpcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpcon = main;
} else {
	ztpcon = tmp;
}


// EXPORTS //

module.exports = ztpcon;

// exports: { "ndarray": "ztpcon.ndarray" }
