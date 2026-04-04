
'use strict';

/**
* Applies an elementary reflector to a general rectangular matrix with loop unrolling when the reflector has order at most 10.
*
* @module @stdlib/lapack/base/zlarfx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var zlarfx = require( '@stdlib/lapack/base/zlarfx' );
*
* var v = new Complex128Array( [ 1.0, 0.0, 0.5, 0.3 ] );
* var tau = new Complex128( 1.6, -0.2 );
* var C = new Complex128Array( [ 1.0, 2.0, 4.0, -1.0, 2.0, 0.5, 5.0, 3.0 ] );
* var WORK = new Complex128Array( 10 );
*
* zlarfx.ndarray( 'left', 2, 2, v, 1, 0, tau, C, 1, 2, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlarfx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlarfx = main;
} else {
	zlarfx = tmp;
}


// EXPORTS //

module.exports = zlarfx;

// exports: { "ndarray": "zlarfx.ndarray" }
