
'use strict';

/**
* Generates a vector of random numbers from a uniform distribution.
*
* @module @stdlib/lapack/base/dlaruv
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

var dlaruv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaruv = main;
} else {
	dlaruv = tmp;
}


// EXPORTS //

module.exports = dlaruv;

// exports: { "ndarray": "dlaruv.ndarray" }
