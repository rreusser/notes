
'use strict';

/**
* Complex blocked RQ factorization.
*
* @module @stdlib/lapack/base/zgerqf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgerqf = require( '@stdlib/lapack/base/zgerqf' );
*
* var A = new Complex128Array( [ 1.0, 0.5, 2.0, 1.0, 3.0, 1.5, 4.0, 2.0, 0.5, 1.0, 1.0, 0.5, 1.5, 1.0, 2.0, 1.5, 1.0, 0.0, 2.0, 0.5, 3.0, 1.0, 4.0, 1.5 ] );
* var TAU = new Complex128Array( 3 );
* var WORK = new Complex128Array( 256 );
*
* zgerqf( 'column-major', 4, 3, A, 4, TAU, 1, WORK, 1, -1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgerqf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgerqf = main;
} else {
	zgerqf = tmp;
}


// EXPORTS //

module.exports = zgerqf;

// exports: { "ndarray": "zgerqf.ndarray" }
