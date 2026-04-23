
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a complex general matrix.
*
* @module @stdlib/lapack/base/zla_gerpvgrw
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

var zla_gerpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_gerpvgrw = main;
} else {
	zla_gerpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_gerpvgrw;

// exports: { "ndarray": "zla_gerpvgrw.ndarray" }
