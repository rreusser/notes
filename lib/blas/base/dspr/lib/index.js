
'use strict';

/**
* Perform symmetric rank-1 update of a packed matrix.
*
* @module @stdlib/blas/base/dspr
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspr = require( '@stdlib/blas/base/dspr' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var AP = new Float64Array( [ 1.0, 0.0, 1.0 ] );
*
* dspr.ndarray( 'upper', 2, 1.0, x, 1, 0, AP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspr = main;
} else {
	dspr = tmp;
}


// EXPORTS //

module.exports = dspr;

// exports: { "ndarray": "dspr.ndarray" }
