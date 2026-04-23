
'use strict';

/**
* Computes the inverse of a complex symmetric matrix in packed storage using the factorization computed by zsptrf.
*
* @module @stdlib/lapack/base/zsptri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
*
* var AP = new Complex128Array( [ 4.0, 2.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var WORK = new Complex128Array( 1 );
*
* var info = zsptri( 'upper', 1, AP, IPIV, WORK );
* // throws <ReferenceError>
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsptri;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsptri = main;
} else {
	zsptri = tmp;
}


// EXPORTS //

module.exports = zsptri;

// exports: { "ndarray": "zsptri.ndarray" }
