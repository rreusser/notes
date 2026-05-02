
'use strict';

/**
* Interchange two vectors.
*
* @module @stdlib/blas/base/dswap
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dswap = require( '@stdlib/blas/base/dswap' );
*
* var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );
*
* dswap.ndarray( 3, x, 1, 0, y, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dswap;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dswap = main;
} else {
	dswap = tmp;
}


// EXPORTS //

module.exports = dswap;

// exports: { "ndarray": "dswap.ndarray" }
