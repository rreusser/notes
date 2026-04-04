
'use strict';

/**
* Find the index of element with maximum absolute value.
*
* @module @stdlib/blas/base/idamax
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

var idamax;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	idamax = main;
} else {
	idamax = tmp;
}


// EXPORTS //

module.exports = idamax;

// exports: { "ndarray": "idamax.ndarray" }
