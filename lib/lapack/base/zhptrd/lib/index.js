
'use strict';

/**
* Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.
*
* @module @stdlib/lapack/base/zhptrd
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zhptrd = require( '@stdlib/lapack/base/zhptrd' );
*
* // 2x2 Hermitian [[3,1-i],[1+i,1]] upper packed: [(3,0), (1,-1), (1,0)]
* var AP = new Complex128Array( [ 3, 0, 1, -1, 1, 0 ] );
* var d = new Float64Array( 2 );
* var e = new Float64Array( 1 );
* var TAU = new Complex128Array( 1 );
*
* zhptrd( 'upper', 2, AP, d, e, TAU );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhptrd;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhptrd = main;
} else {
	zhptrd = tmp;
}


// EXPORTS //

module.exports = zhptrd;

// exports: { "ndarray": "zhptrd.ndarray" }
