
'use strict';

/**
* Iterative refinement for a general tridiagonal system.
*
* @module @stdlib/lapack/base/dgtrfs
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

var dgtrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtrfs = main;
} else {
	dgtrfs = tmp;
}


// EXPORTS //

module.exports = dgtrfs;

// exports: { "ndarray": "dgtrfs.ndarray" }
