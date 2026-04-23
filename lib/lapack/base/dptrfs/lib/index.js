
'use strict';

/**
* Improves solution to a real tridiagonal system and provides error bounds.
*
* @module @stdlib/lapack/base/dptrfs
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

var dptrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dptrfs = main;
} else {
	dptrfs = tmp;
}


// EXPORTS //

module.exports = dptrfs;

// exports: { "ndarray": "dptrfs.ndarray" }
