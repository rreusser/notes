
'use strict';

/**
* Improves the solution to A*X = B using iterative refinement.
*
* @module @stdlib/lapack/base/dgerfs
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

var dgerfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgerfs = main;
} else {
	dgerfs = tmp;
}


// EXPORTS //

module.exports = dgerfs;

// exports: { "ndarray": "dgerfs.ndarray" }
