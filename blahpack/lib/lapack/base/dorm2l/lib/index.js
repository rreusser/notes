
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from QL factorization (unblocked).
*
* @module @stdlib/lapack/base/dorm2l
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

var dorm2l;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dorm2l = main;
} else {
	dorm2l = tmp;
}


// EXPORTS //

module.exports = dorm2l;

// exports: { "ndarray": "dorm2l.ndarray" }
