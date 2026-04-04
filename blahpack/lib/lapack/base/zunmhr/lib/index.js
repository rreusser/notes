

'use strict';

/**
* Multiplies a matrix by the unitary matrix Q from Hessenberg reduction
*
* @module @stdlib/lapack/base/zunmhr
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

var zunmhr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmhr = main;
} else {
	zunmhr = tmp;
}


// EXPORTS //

module.exports = zunmhr;

// exports: { "ndarray": "zunmhr.ndarray" }
