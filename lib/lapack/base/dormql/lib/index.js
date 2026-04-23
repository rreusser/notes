
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from QL factorization (blocked).
*
* @module @stdlib/lapack/base/dormql
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

var dormql;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormql = main;
} else {
	dormql = tmp;
}


// EXPORTS //

module.exports = dormql;

// exports: { "ndarray": "dormql.ndarray" }
