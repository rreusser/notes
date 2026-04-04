
'use strict';

/**
* Generates the orthogonal matrix Q from Hessenberg reduction.
*
* @module @stdlib/lapack/base/dorghr
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

var dorghr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dorghr = main;
} else {
	dorghr = tmp;
}


// EXPORTS //

module.exports = dorghr;

// exports: { "ndarray": "dorghr.ndarray" }
