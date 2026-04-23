
'use strict';

/**
* Estimates the reciprocal of the condition number of a complex Hermitian matrix in packed storage.
*
* @module @stdlib/lapack/base/zhpcon
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhpcon = require( '@stdlib/lapack/base/zhpcon' );
*
* // 2x2 identity in upper packed format: A(1,1)=1, A(1,2)=0, A(2,2)=1
* var AP = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var rcond = new Float64Array( 1 );
* var WORK = new Complex128Array( 4 );
*
* var info = zhpcon.ndarray( 'upper', 2, AP, 1, 0, IPIV, 1, 0, 1.0, rcond, WORK, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpcon = main;
} else {
	zhpcon = tmp;
}


// EXPORTS //

module.exports = zhpcon;

// exports: { "ndarray": "zhpcon.ndarray" }
