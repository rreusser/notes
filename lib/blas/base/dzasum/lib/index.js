
'use strict';

/**
* Compute the sum of absolute values of a complex vector.
*
* @module @stdlib/blas/base/dzasum
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dzasum = require( '@stdlib/blas/base/dzasum' );
*
* var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dzasum.ndarray( 2, zx, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dzasum;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dzasum = main;
} else {
	dzasum = tmp;
}


// EXPORTS //

module.exports = dzasum;

// exports: { "ndarray": "dzasum.ndarray" }
