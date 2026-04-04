
'use strict';

/**
* Improve solution to complex linear system with iterative refinement.
*
* @module @stdlib/lapack/base/zgerfs
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

var zgerfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgerfs = main;
} else {
	zgerfs = tmp;
}


// EXPORTS //

module.exports = zgerfs;

// exports: { "ndarray": "zgerfs.ndarray" }
