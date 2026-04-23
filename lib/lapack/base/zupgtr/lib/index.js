

'use strict';

/**
* Generates an orthogonal matrix Q which is defined as the product of n-1 elementary reflectors of order n, as returned by zhptrd.
*
* @module @stdlib/lapack/base/zupgtr
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zupgtr = require( '@stdlib/lapack/base/zupgtr' );
*
* var AP = new Complex128Array( 3 );
* var TAU = new Complex128Array( 1 );
* var Q = new Complex128Array( 4 );
* var WORK = new Complex128Array( 4 );
*
* zupgtr( 'column-major', 'upper', 2, AP, TAU, Q, 2, WORK );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zupgtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zupgtr = main;
} else {
	zupgtr = tmp;
}


// EXPORTS //

module.exports = zupgtr;

// exports: { "ndarray": "zupgtr.ndarray" }
