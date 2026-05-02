
'use strict';

/**
* Compute the dot product of two vectors with extended precision accumulation.
*
* @module @stdlib/blas/base/dsdot
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsdot = require( '@stdlib/blas/base/dsdot' );
*
* var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );
*
* dsdot.ndarray( 3, x, 1, 0, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsdot;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsdot = main;
} else {
	dsdot = tmp;
}


// EXPORTS //

module.exports = dsdot;

// exports: { "ndarray": "dsdot.ndarray" }
