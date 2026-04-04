
'use strict';

/**
* Computes the inverse of a complex triangular matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/ztftri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztftri = require( '@stdlib/lapack/base/ztftri' );
*
* var a = new Complex128Array( [ 2, 1, 0.5, -0.3, 1, 0.5, 4, 0, 3, -1, 0.8, 0.2 ] );
* var info = ztftri.ndarray( 'no-transpose', 'lower', 'non-unit', 3, a, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztftri.ndarray" }
