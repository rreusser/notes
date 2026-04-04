
'use strict';

/**
* Compute the solution to a complex system of linear equations A * X = B.
*
* @module @stdlib/lapack/base/zgesv
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

var zgesv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgesv = main;
} else {
	zgesv = tmp;
}


// EXPORTS //

module.exports = zgesv;

// exports: { "ndarray": "zgesv.ndarray" }
