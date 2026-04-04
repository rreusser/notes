

'use strict';

/**
* Compute row and column scalings to equilibrate a complex Hermitian positive definite matrix in packed storage
*
* @module @stdlib/lapack/base/zppequ
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

var zppequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zppequ = main;
} else {
	zppequ = tmp;
}


// EXPORTS //

module.exports = zppequ;

// exports: { "ndarray": "zppequ.ndarray" }
