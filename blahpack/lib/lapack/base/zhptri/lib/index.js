
'use strict';

/**
* Computes the inverse of a complex Hermitian matrix in packed storage using the factorization computed by zhptrf.
*
* @module @stdlib/lapack/base/zhptri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
*
* var AP = new Complex128Array( [ 4.0, 0.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var WORK = new Complex128Array( 1 );
*
* var info = zhptri( 'upper', 1, AP, IPIV, WORK );
* // throws <ReferenceError>
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhptri;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhptri = main;
} else {
	zhptri = tmp;
}


// EXPORTS //

module.exports = zhptri;

// exports: { "ndarray": "zhptri.ndarray" }
