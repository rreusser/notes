
'use strict';

/**
* Computes all eigenvalues and optionally eigenvectors of a real symmetric-definite generalized eigenproblem in packed storage.
*
* @module @stdlib/lapack/base/dspgv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspgv = require( '@stdlib/lapack/base/dspgv' );
*
* // A = [4 2 1; 2 5 3; 1 3 6] in upper packed storage:
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
*
* // B = [4 2 0; 2 5 1; 0 1 3] in upper packed storage:
* var BP = new Float64Array( [ 4.0, 2.0, 5.0, 0.0, 1.0, 3.0 ] );
*
* var W = new Float64Array( 3 );
* var Z = new Float64Array( 9 );
* var WORK = new Float64Array( 9 );
*
* var info = dspgv( 'column-major', 1, 'compute-vectors', 'upper', 3, AP, BP, W, Z, 3, WORK );
* // info => 0
* // W => [ ~0.729, 1.0, ~2.089 ]
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspgv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspgv = main;
} else {
	dspgv = tmp;
}


// EXPORTS //

module.exports = dspgv;

// exports: { "ndarray": "dspgv.ndarray" }
