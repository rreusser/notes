
'use strict';

/**
* Improves the computed solution to a complex system A * X = B where A is general band and provides error bounds.
*
* @module @stdlib/lapack/base/zgbrfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zgbrfs = require( '@stdlib/lapack/base/zgbrfs' );
*
* var AB = new Complex128Array( 1 );
* var AFB = new Complex128Array( 1 );
* var IPIV = new Int32Array( [ 0 ] );
* var B = new Complex128Array( 1 );
* var X = new Complex128Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 2 );
* var RWORK = new Float64Array( 1 );
*
* var info = zgbrfs.ndarray( 'no-transpose', 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zgbrfs.ndarray" }
