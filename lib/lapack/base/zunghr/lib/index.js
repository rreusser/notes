

'use strict';

/**
* Generates the unitary matrix Q from Hessenberg reduction
*
* @module @stdlib/lapack/base/zunghr
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

var zunghr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunghr = main;
} else {
	zunghr = tmp;
}


// EXPORTS //

module.exports = zunghr;

// exports: { "ndarray": "zunghr.ndarray" }
