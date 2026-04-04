
'use strict';

/**
* Returns the norm of a real upper Hessenberg matrix.
*
* @module @stdlib/lapack/base/dlanhs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlanhs = require( '@stdlib/lapack/base/dlanhs' );
*
* var A = new Float64Array( [ 1.0, 4.0, 0.0, 2.0, 5.0, 7.0, 3.0, 6.0, 8.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlanhs( 'column-major', 'one-norm', 3, A, 3, WORK, 1 );
* // returns 17.0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlanhs = require( '@stdlib/lapack/base/dlanhs' );
*
* var A = new Float64Array( [ 1.0, 4.0, 0.0, 2.0, 5.0, 7.0, 3.0, 6.0, 8.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlanhs.ndarray( 'one-norm', 3, A, 1, 3, 0, WORK, 1, 0 );
* // returns 17.0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlanhs.ndarray" }
