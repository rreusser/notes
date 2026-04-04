
'use strict';

/**
* Solves a system of linear equations with a real symmetric matrix in packed storage using the factorization computed by dsptrf.
*
* @module @stdlib/lapack/base/dsptrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsptrs = require( '@stdlib/lapack/base/dsptrs' );
*
* // Pre-factored 2x2 diagonal matrix (lower packed, already factored):
* var AP = new Float64Array( [ 2.0, 0.0, 3.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var B = new Float64Array( [ 4.0, 9.0 ] );
*
* dsptrs( 'lower', 2, 1, AP, IPIV, B, 2 );
* // B => <Float64Array>[ 2.0, 3.0 ]
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsptrs = require( '@stdlib/lapack/base/dsptrs' );
*
* var AP = new Float64Array( [ 2.0, 0.0, 3.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var B = new Float64Array( [ 4.0, 9.0 ] );
*
* dsptrs.ndarray( 'lower', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
* // B => <Float64Array>[ 2.0, 3.0 ]
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsptrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsptrs = main;
} else {
	dsptrs = tmp;
}


// EXPORTS //

module.exports = dsptrs;

// exports: { "ndarray": "dsptrs.ndarray" }
