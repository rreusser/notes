
'use strict';

/**
* Construct a modified Givens plane rotation.
*
* @module @stdlib/blas/base/drotmg
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var drotmg = require( '@stdlib/blas/base/drotmg' );
*
* var D = new Float64Array( [ 1.0, 1.0 ] );
* var x1 = new Float64Array( [ 3.0 ] );
* var param = new Float64Array( 5 );
*
* drotmg.ndarray( D, 1, 0, x1, 1, 0, 4.0, param, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var drotmg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	drotmg = main;
} else {
	drotmg = tmp;
}


// EXPORTS //

module.exports = drotmg;

// exports: { "ndarray": "drotmg.ndarray" }
