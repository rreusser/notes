
'use strict';

/**
* Generates a vector of complex plane rotations with real cosines and complex sines.
*
* @module @stdlib/lapack/base/zlargv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlargv = require( '@stdlib/lapack/base/zlargv' );
*
* var x = new Complex128Array( [ 3.0, 1.0 ] );
* var y = new Complex128Array( [ 4.0, 0.0 ] );
* var c = new Float64Array( 1 );
*
* zlargv.ndarray( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlargv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlargv = main;
} else {
	zlargv = tmp;
}


// EXPORTS //

module.exports = zlargv;

// exports: { "ndarray": "zlargv.ndarray" }
