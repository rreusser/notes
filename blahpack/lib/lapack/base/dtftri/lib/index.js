
'use strict';

/**
* Computes the inverse of a real triangular matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/dtftri
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtftri = require( '@stdlib/lapack/base/dtftri' );
*
* // 3x3 lower triangular matrix in RFP format:
* var A = new Float64Array( [ 4, 1, 1.5, 9, 7, 2.5 ] );
* var info = dtftri( 'no-transpose', 'lower', 'non-unit', 3, A );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtftri;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtftri = main;
} else {
	dtftri = tmp;
}


// EXPORTS //

module.exports = dtftri;

// exports: { "ndarray": "dtftri.ndarray" }
