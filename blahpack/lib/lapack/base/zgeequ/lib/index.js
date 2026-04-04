
'use strict';

/**
* Compute row and column scalings for a complex general matrix.
*
* @module @stdlib/lapack/base/zgeequ
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

var zgeequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgeequ = main;
} else {
	zgeequ = tmp;
}


// EXPORTS //

module.exports = zgeequ;

// exports: { "ndarray": "zgeequ.ndarray" }
