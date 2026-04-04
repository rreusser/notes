
'use strict';

/**
* Computes row and column scalings to equilibrate a complex general band matrix.
*
* @module @stdlib/lapack/base/zgbequ
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zgbequ = require( '@stdlib/lapack/base/zgbequ' );
*
* var AB = new Complex128Array( [ 3, 4, 1, 0, 0, 2 ] );
* var r = new Float64Array( 3 );
* var c = new Float64Array( 3 );
* var out = zgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
* // out.info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgbequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbequ = main;
} else {
	zgbequ = tmp;
}


// EXPORTS //

module.exports = zgbequ;

// exports: { "ndarray": "zgbequ.ndarray" }
