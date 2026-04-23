
'use strict';

/**
* Compute row and column scalings to equilibrate a symmetric positive definite matrix in packed storage.
*
* @module @stdlib/lapack/base/dppequ
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

var dppequ;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dppequ = main;
} else {
	dppequ = tmp;
}


// EXPORTS //

module.exports = dppequ;

// exports: { "ndarray": "dppequ.ndarray" }
