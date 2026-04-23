

'use strict';

/**
* Iterative refinement for Hermitian positive definite system
*
* @module @stdlib/lapack/base/zporfs
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

var zporfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zporfs = main;
} else {
	zporfs = tmp;
}


// EXPORTS //

module.exports = zporfs;

// exports: { "ndarray": "zporfs.ndarray" }
