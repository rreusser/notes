
'use strict';

/**
* Solves a real symmetric indefinite system of linear equations using Bunch-Kaufman factorization.
*
* @module @stdlib/lapack/base/dsysv
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

var dsysv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsysv = main;
} else {
	dsysv = tmp;
}


// EXPORTS //

module.exports = dsysv;

// exports: { "ndarray": "dsysv.ndarray" }
