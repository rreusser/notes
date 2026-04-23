
'use strict';

/**
* Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite in packed storage and provides error bounds.
*
* @module @stdlib/lapack/base/zpprfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zpprfs = require( '@stdlib/lapack/base/zpprfs' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 3.0, 0.0 ] );
* var AFP = new Complex128Array( [ 2.0, 0.0, 0.5, -0.5, 1.5811, 0.0 ] );
* var B = new Complex128Array( [ 6.0, 2.0, 5.0, 3.0 ] );
* var X = new Complex128Array( [ 1.0, 1.0, 1.0, 1.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 4 );
* var RWORK = new Float64Array( 2 );
*
* zpprfs( 'upper', 2, 1, AP, AFP, B, 2, X, 2, FERR, BERR, WORK, RWORK );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zpprfs = require( '@stdlib/lapack/base/zpprfs' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 3.0, 0.0 ] );
* var AFP = new Complex128Array( [ 2.0, 0.0, 0.5, -0.5, 1.5811, 0.0 ] );
* var B = new Complex128Array( [ 6.0, 2.0, 5.0, 3.0 ] );
* var X = new Complex128Array( [ 1.0, 1.0, 1.0, 1.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 4 );
* var RWORK = new Float64Array( 2 );
*
* zpprfs.ndarray( 'upper', 2, 1, AP, 1, 0, AFP, 1, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpprfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpprfs = main;
} else {
	zpprfs = tmp;
}


// EXPORTS //

module.exports = zpprfs;

// exports: { "ndarray": "zpprfs.ndarray" }
