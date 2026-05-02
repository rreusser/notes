
'use strict';

/**
* Construct a Givens plane rotation.
*
* @module @stdlib/blas/base/drotg
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var drotg = require( '@stdlib/blas/base/drotg' );
*
* var ab = new Float64Array( [ 3.0, 4.0 ] );
* var cs = new Float64Array( 2 );
*
* drotg.ndarray( ab, 1, 0, cs, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var drotg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	drotg = main;
} else {
	drotg = tmp;
}


// EXPORTS //

module.exports = drotg;

// exports: { "ndarray": "drotg.ndarray" }
