
'use strict';

/**
* Returns the norm of a complex general band matrix.
*
* @module @stdlib/lapack/base/zlangb
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlangb = require( '@stdlib/lapack/base/zlangb' );
*
* var AB = new Complex128Array( [ 0, 0, 1, 2, 7, 8, 0, 0, 3, 4, 9, 1, 6, 7, 5, 6, 2, 3, 8, 9, 5, 6, 4, 5, 1, 2, 7, 8, 2, 3, 3, 4, 9, 1, 4, 5, 0, 0 ] );
* var WORK = new Float64Array( 5 );
*
* var v = zlangb.ndarray( 'one-norm', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
* // returns ~31.27
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlangb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlangb = main;
} else {
	zlangb = tmp;
}


// EXPORTS //

module.exports = zlangb;

// exports: { "ndarray": "zlangb.ndarray" }
