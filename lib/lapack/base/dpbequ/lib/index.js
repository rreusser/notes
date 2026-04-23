
'use strict';

/**
* Compute row and column scalings to equilibrate a symmetric positive definite band matrix.
*
* @module @stdlib/lapack/base/dpbequ
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

var dpbequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpbequ = main;
} else {
	dpbequ = tmp;
}


// EXPORTS //

module.exports = dpbequ;

// exports: { "ndarray": "dpbequ.ndarray" }
