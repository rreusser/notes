'use strict';

/**
* Merge two sets of singular values together into a single sorted set and deflate.
*
* @module @stdlib/lapack/base/dlasd7
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

var dlasd7;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasd7 = main;
} else {
	dlasd7 = tmp;
}


// EXPORTS //

module.exports = dlasd7;

// exports: { "ndarray": "dlasd7.ndarray" }
