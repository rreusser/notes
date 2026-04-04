

'use strict';

/**
* Improves solution to a complex Hermitian tridiagonal system and provides error bounds
*
* @module @stdlib/lapack/base/zptrfs
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

var zptrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zptrfs = main;
} else {
	zptrfs = tmp;
}


// EXPORTS //

module.exports = zptrfs;

// exports: { "ndarray": "zptrfs.ndarray" }
