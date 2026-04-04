'use strict';

/**
* Returns the norm of a real triangular band matrix.
*
* @module @stdlib/lapack/base/dlantb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlantb = require( '@stdlib/lapack/base/dlantb' );
*
* // Upper triangular 4x4 band matrix with K=1:
* var AB = new Float64Array( [ 0, 2, -3, 4, 1, -5, 6, 7 ] );
* var WORK = new Float64Array( 4 );
*
* var result = dlantb.ndarray( 'one-norm', 'upper', 'non-unit', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
* // returns 13.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlantb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlantb = main;
} else {
	dlantb = tmp;
}


// EXPORTS //

module.exports = dlantb;

// exports: { "ndarray": "dlantb.ndarray" }
