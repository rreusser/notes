

'use strict';

/**
* Compute row and column scalings to equilibrate a complex Hermitian positive definite band matrix.
*
* @module @stdlib/lapack/base/zpbequ
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

var zpbequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpbequ = main;
} else {
	zpbequ = tmp;
}


// EXPORTS //

module.exports = zpbequ;

// exports: { "ndarray": "zpbequ.ndarray" }
