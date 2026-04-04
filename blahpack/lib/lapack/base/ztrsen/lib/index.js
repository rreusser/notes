

'use strict';

/**
* Reorder Schur factorization and compute condition numbers
*
* @module @stdlib/lapack/base/ztrsen
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

var ztrsen;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrsen = main;
} else {
	ztrsen = tmp;
}


// EXPORTS //

module.exports = ztrsen;

// exports: { "ndarray": "ztrsen.ndarray" }
