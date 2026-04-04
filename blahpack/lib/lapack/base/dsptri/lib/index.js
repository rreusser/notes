
'use strict';

/**
* Computes the inverse of a real symmetric matrix stored in packed format.
*
* @module @stdlib/lapack/base/dsptri
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
* var dsptri = require( '@stdlib/lapack/base/dsptri' );
*
* // 3x3 SPD matrix, upper packed: [4,2,5,1,3,6]
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var IPIV = new Int32Array( 3 );
*
* dsptrf( 'upper', 3, AP, IPIV );
* var info = dsptri( 'upper', 3, AP, IPIV );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsptri;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsptri = main;
} else {
	dsptri = tmp;
}


// EXPORTS //

module.exports = dsptri;

// exports: { "ndarray": "dsptri.ndarray" }
