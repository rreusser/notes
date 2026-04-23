
'use strict';

/**
* Performs symmetric matrix-matrix multiplication.
*
* @module @stdlib/blas/base/dsymm
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

var dsymm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsymm = main;
} else {
	dsymm = tmp;
}


// EXPORTS //

module.exports = dsymm;

// exports: { "ndarray": "dsymm.ndarray" }
