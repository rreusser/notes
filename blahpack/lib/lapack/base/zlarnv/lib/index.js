
'use strict';

/**
* Returns a vector of complex random numbers from a uniform or normal distribution.
*
* @module @stdlib/lapack/base/zlarnv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zlarnv = require( '@stdlib/lapack/base/zlarnv' );
*
* var iseed = new Int32Array( [ 1, 2, 3, 4 ] );
* var x = new Complex128Array( 5 );
*
* zlarnv.ndarray( 1, iseed, 1, 0, 5, x, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlarnv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlarnv = main;
} else {
	zlarnv = tmp;
}


// EXPORTS //

module.exports = zlarnv;

// exports: { "ndarray": "zlarnv.ndarray" }
