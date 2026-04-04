
'use strict';

/**
* Multiplies a matrix by the orthogonal matrix Q from Hessenberg reduction.
*
* @module @stdlib/lapack/base/dormhr
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

var dormhr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormhr = main;
} else {
	dormhr = tmp;
}


// EXPORTS //

module.exports = dormhr;

// exports: { "ndarray": "dormhr.ndarray" }
