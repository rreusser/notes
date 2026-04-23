
'use strict';

/**
* Returns the norm of a real general band matrix.
*
* @module @stdlib/lapack/base/dlangb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlangb = require( '@stdlib/lapack/base/dlangb' );
*
* var AB = new Float64Array( [ 0, 0, 1, 7, 0, 3, 9, 6, 5, 2, 8, 5, 4, 1, 7, 2, 3, 9, 4, 0 ] );
* var WORK = new Float64Array( 5 );
*
* var v = dlangb.ndarray( 'one-norm', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
* // returns 20.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlangb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlangb = main;
} else {
	dlangb = tmp;
}


// EXPORTS //

module.exports = dlangb;

// exports: { "ndarray": "dlangb.ndarray" }
