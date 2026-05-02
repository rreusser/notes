
'use strict';

/**
* Construct a Givens plane rotation with real cosine and complex sine.
*
* @module @stdlib/blas/base/zrotg
*
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zrotg = require( '@stdlib/blas/base/zrotg' );
*
* var out = new Float64Array( 8 );
*
* zrotg.ndarray( 3.0, 0.0, 4.0, 0.0, out, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zrotg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zrotg = main;
} else {
	zrotg = tmp;
}


// EXPORTS //

module.exports = zrotg;

// exports: { "ndarray": "zrotg.ndarray" }
