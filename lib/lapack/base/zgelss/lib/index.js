
'use strict';

/**
* Computes the minimum norm solution to a complex linear least squares problem using SVD.
*
* @module @stdlib/lapack/base/zgelss
*
* @example
* // TODO: Add example
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgelss;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgelss = main;
} else {
	zgelss = tmp;
}


// EXPORTS //

module.exports = zgelss;

// exports: { "ndarray": "zgelss.ndarray" }
