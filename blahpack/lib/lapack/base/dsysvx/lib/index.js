
'use strict';

/**
* Expert symmetric indefinite solver with condition estimation and iterative refinement.
*
* @module @stdlib/lapack/base/dsysvx
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

var dsysvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsysvx = main;
} else {
	dsysvx = tmp;
}


// EXPORTS //

module.exports = dsysvx;

// exports: { "ndarray": "dsysvx.ndarray" }
