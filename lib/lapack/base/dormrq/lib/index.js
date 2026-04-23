
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from RQ factorization (blocked).
*
* @module @stdlib/lapack/base/dormrq
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

var dormrq;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormrq = main;
} else {
	dormrq = tmp;
}


// EXPORTS //

module.exports = dormrq;

// exports: { "ndarray": "dormrq.ndarray" }
