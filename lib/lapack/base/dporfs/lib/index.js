
'use strict';

/**
* Improves solution to a symmetric positive definite system and provides error bounds.
*
* @module @stdlib/lapack/base/dporfs
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

var dporfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dporfs = main;
} else {
	dporfs = tmp;
}


// EXPORTS //

module.exports = dporfs;

// exports: { "ndarray": "dporfs.ndarray" }
