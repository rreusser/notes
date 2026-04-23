
'use strict';

/**
* Computes the generalized singular value decomposition of a real matrix pair.
*
* @module @stdlib/lapack/base/dggsvd3
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

var dggsvd3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dggsvd3 = main;
} else {
	dggsvd3 = tmp;
}


// EXPORTS //

module.exports = dggsvd3;

// exports: { "ndarray": "dggsvd3.ndarray" }
