
'use strict';

/**
* Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/ztfsm
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var ztfsm = require( '@stdlib/lapack/base/ztfsm' );
*
* var alpha = new Complex128( 1.0, 0.0 );
* var A = new Complex128Array( [ 4, 0, 2, 1, 3, 1.5, 9, 0, 7, 0, 5, 2.5 ] );
* var B = new Complex128Array( [ 1, 0.3, 2, 0.6, 3, 0.9 ] );
* ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 1, alpha, A, B );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztfsm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztfsm = main;
} else {
	ztfsm = tmp;
}


// EXPORTS //

module.exports = ztfsm;

// exports: { "ndarray": "ztfsm.ndarray" }
