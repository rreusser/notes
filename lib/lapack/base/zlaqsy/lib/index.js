
'use strict';

/**
* Scales a symmetric/Hermitian matrix using scaling factors computed by zpoequ.
*
* @module @stdlib/lapack/base/zlaqsy
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaqsy = require( '@stdlib/lapack/base/zlaqsy' );
*
* var A = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 1.0, 0.5, 9.0, 2.0 ] );
* var s = new Float64Array( [ 0.5, 0.25 ] );
*
* var equed = zlaqsy.ndarray( 'upper', 2, A, 1, 2, 0, s, 1, 0, 0.05, 9.0 );
* // returns 'yes'
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqsy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqsy = main;
} else {
	zlaqsy = tmp;
}


// EXPORTS //

module.exports = zlaqsy;

// exports: { "ndarray": "zlaqsy.ndarray" }
