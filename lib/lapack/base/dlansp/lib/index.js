
'use strict';

/**
* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real symmetric matrix supplied in packed storage.
*
* @module @stdlib/lapack/base/dlansp
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlansp = require( '@stdlib/lapack/base/dlansp' );
*
* var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlansp( 'max', 'upper', 3, AP, WORK );
* // returns 7.0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlansp = require( '@stdlib/lapack/base/dlansp' );
*
* var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlansp.ndarray( 'max', 'upper', 3, AP, 1, 0, WORK, 1, 0 );
* // returns 7.0
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlansp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlansp = main;
} else {
	dlansp = tmp;
}


// EXPORTS //

module.exports = dlansp;

// exports: { "ndarray": "dlansp.ndarray" }
