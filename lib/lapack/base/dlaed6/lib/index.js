
'use strict';

/**
* Compute one Newton step in the solution of the secular equation.
*
* @module @stdlib/lapack/base/dlaed6
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlaed6 = require( '@stdlib/lapack/base/dlaed6' );
*
* var d = new Float64Array( [ 1.0, 3.0, 7.0 ] );
* var z = new Float64Array( [ 0.3, 0.5, 0.8 ] );
* var tau = new Float64Array( 1 );
* var rho = 0.5;
* var finit = rho + (z[0]/d[0]) + (z[1]/d[1]) + (z[2]/d[2]);
*
* dlaed6( 2, true, rho, d, z, finit, tau );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaed6;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaed6 = main;
} else {
	dlaed6 = tmp;
}


// EXPORTS //

module.exports = dlaed6;

// exports: { "ndarray": "dlaed6.ndarray" }
