
'use strict';

/**
* Equilibrates a complex Hermitian matrix in packed storage using scaling factors.
*
* @module @stdlib/lapack/base/zlaqhp
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaqhp = require( '@stdlib/lapack/base/zlaqhp' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 9.0, 0.0 ] );
* var S = new Float64Array( [ 0.5, 0.25 ] );
*
* var equed = zlaqhp( 'upper', 2, AP, S, 1, 0.05, 9.0 );
* // returns 'yes'
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlaqhp.ndarray" }
