

'use strict';

/**
* Compute row/column scaling for Hermitian positive definite matrix
*
* @module @stdlib/lapack/base/zpoequ
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

var zpoequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpoequ = main;
} else {
	zpoequ = tmp;
}


// EXPORTS //

module.exports = zpoequ;

// exports: { "ndarray": "zpoequ.ndarray" }
