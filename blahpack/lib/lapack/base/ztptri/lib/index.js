
'use strict';

/**
* Computes the inverse of a complex triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/ztptri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztptri = require( '@stdlib/lapack/base/ztptri' );
*
* // 2x2 upper triangular: A = [ (2,1) (1,0); (0,0) (3,-1) ]
* var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.0, 3.0, -1.0 ] );
*
* var info = ztptri( 'upper', 'non-unit', 2, AP );
* // info => 0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztptri = require( '@stdlib/lapack/base/ztptri' );
*
* // 2x2 upper triangular: A = [ (2,1) (1,0); (0,0) (3,-1) ]
* var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.0, 3.0, -1.0 ] );
*
* var info = ztptri.ndarray( 'upper', 'non-unit', 2, AP, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztptri.ndarray" }
