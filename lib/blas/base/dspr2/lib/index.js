
'use strict';

/**
* Perform symmetric rank-2 update of a packed matrix.
*
* @module @stdlib/blas/base/dspr2
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspr2 = require( '@stdlib/blas/base/dspr2' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var y = new Float64Array( [ 3.0, 4.0 ] );
* var AP = new Float64Array( [ 1.0, 0.0, 1.0 ] );
*
* dspr2.ndarray( 'upper', 2, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspr2 = main;
} else {
	dspr2 = tmp;
}


// EXPORTS //

module.exports = dspr2;

// exports: { "ndarray": "dspr2.ndarray" }
