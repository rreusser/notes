'use strict';

/**
* Returns the norm of a complex Hermitian band matrix.
*
* @module @stdlib/lapack/base/zlanhb
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlanhb = require( '@stdlib/lapack/base/zlanhb' );
*
* var AB = new Complex128Array( [ 0, 0, 2, 0, -3, 1, 4, 0, 1, 2, -5, 0, 6, -3, 7, 0 ] );
* var WORK = new Float64Array( 4 );
*
* var result = zlanhb( 'max', 'upper', 4, 1, AB, 2, WORK );
* // returns 7.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlanhb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlanhb = main;
} else {
	zlanhb = tmp;
}


// EXPORTS //

module.exports = zlanhb;

// exports: { "ndarray": "zlanhb.ndarray" }
