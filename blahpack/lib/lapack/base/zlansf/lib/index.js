
'use strict';

/**
* Returns the norm of a complex symmetric matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/zlansf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlansf = require( '@stdlib/lapack/base/zlansf' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 5.0, -1.0, 2.0, 1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = zlansf( 'max', 'no-transpose', 'upper', 3, A, WORK );
* // returns ~5.099
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlansf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlansf = main;
} else {
	zlansf = tmp;
}


// EXPORTS //

module.exports = zlansf;

// exports: { "ndarray": "zlansf.ndarray" }
