
'use strict';

/**
* Compute unconjugated dot product of two complex vectors.
*
* @module @stdlib/blas/base/zdotu
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zdotu = require( '@stdlib/blas/base/zdotu' );
*
* var x = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
* var y = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
*
* zdotu.ndarray( 2, x, 1, 0, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zdotu;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zdotu = main;
} else {
	zdotu = tmp;
}


// EXPORTS //

module.exports = zdotu;

// exports: { "ndarray": "zdotu.ndarray" }
