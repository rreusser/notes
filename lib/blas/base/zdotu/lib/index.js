
'use strict';

/**
* Compute unconjugated dot product of two complex vectors.
*
* @module @stdlib/blas/base/zdotu
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

var zdotu;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zdotu = main;
} else {
	zdotu = tmp;
}


// EXPORTS //

module.exports = zdotu;

// exports: { "ndarray": "zdotu.ndarray" }
