
'use strict';

/**
* Rearrange the rows of a matrix as specified by a permutation vector.
*
* @module @stdlib/lapack/base/dlapmr
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

var dlapmr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlapmr = main;
} else {
	dlapmr = tmp;
}


// EXPORTS //

module.exports = dlapmr;

// exports: { "ndarray": "dlapmr.ndarray" }
