
'use strict';

/**
* Estimates the 1-norm of a square complex matrix using reverse communication.
*
* @module @stdlib/lapack/base/zlacon
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlacon = require( '@stdlib/lapack/base/zlacon' );
*
* var N = 3;
* var V = new Complex128Array( N );
* var X = new Complex128Array( N );
* var EST = new Float64Array( 1 );
* var KASE = new Int32Array( 1 );
*
* KASE[ 0 ] = 0;
* zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
* // KASE[ 0 ] => 1
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlacon.ndarray" }
