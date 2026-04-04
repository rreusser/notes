

'use strict';

/**
* Rearrange the rows of a complex matrix as specified by a permutation vector.
*
* @module @stdlib/lapack/base/zlapmr
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

var zlapmr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlapmr = main;
} else {
	zlapmr = tmp;
}


// EXPORTS //

module.exports = zlapmr;

// exports: { "ndarray": "zlapmr.ndarray" }
