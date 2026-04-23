

'use strict';

/**
* Complex Hermitian iterative refinement
*
* @module @stdlib/lapack/base/zherfs
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

var zherfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zherfs = main;
} else {
	zherfs = tmp;
}


// EXPORTS //

module.exports = zherfs;

// exports: { "ndarray": "zherfs.ndarray" }
