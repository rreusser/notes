
'use strict';

/**
* Find the index of element with maximum absolute value.
*
* @module @stdlib/blas/base/idamax
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var idamax = require( '@stdlib/blas/base/idamax' );
*
* var x = new Float64Array( [ 1.0, -4.0, 3.0, 2.0 ] );
*
* idamax.ndarray( 4, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var idamax;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	idamax = main;
} else {
	idamax = tmp;
}


// EXPORTS //

module.exports = idamax;

// exports: { "ndarray": "idamax.ndarray" }
