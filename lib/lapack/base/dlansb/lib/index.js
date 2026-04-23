'use strict';

/**
* Returns the norm of a real symmetric band matrix.
*
* @module @stdlib/lapack/base/dlansb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlansb = require( '@stdlib/lapack/base/dlansb' );
*
* var AB = new Float64Array( [ 0, 2, -3, 4, 1, -5, 6, 7 ] );
* var WORK = new Float64Array( 4 );
*
* var result = dlansb( 'max', 'upper', 4, 1, AB, 2, WORK );
* // returns 7.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlansb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlansb = main;
} else {
	dlansb = tmp;
}


// EXPORTS //

module.exports = dlansb;

// exports: { "ndarray": "dlansb.ndarray" }
