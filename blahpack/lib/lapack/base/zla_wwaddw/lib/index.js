'use strict';

/**
* Adds a complex vector in doubled-single precision representation.
*
* @module @stdlib/lapack/base/zla_wwaddw
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlaWwaddw = require( '@stdlib/lapack/base/zla_wwaddw' );
*
* var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var y = new Complex128Array( [ 0.1, 0.2, 0.3, 0.4 ] );
* var w = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0 ] );
*
* zlaWwaddw( 2, x, 1, y, 1, w, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlaWwaddw = require( '@stdlib/lapack/base/zla_wwaddw' );
*
* var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var y = new Complex128Array( [ 0.1, 0.2, 0.3, 0.4 ] );
* var w = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0 ] );
*
* zlaWwaddw.ndarray( 2, x, 1, 0, y, 1, 0, w, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaWwaddw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaWwaddw = main;
} else {
	zlaWwaddw = tmp;
}


// EXPORTS //

module.exports = zlaWwaddw;

// exports: { "ndarray": "zla_wwaddw.ndarray" }
