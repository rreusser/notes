

'use strict';

/**
* Solves a complex symmetric indefinite system using Bunch-Kaufman factorization
*
* @module @stdlib/lapack/base/zsysv
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

var zsysv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsysv = main;
} else {
	zsysv = tmp;
}


// EXPORTS //

module.exports = zsysv;

// exports: { "ndarray": "zsysv.ndarray" }
