
'use strict';

/**
* Compute the reciprocal pivot growth factor for a complex Hermitian indefinite matrix.
*
* @module @stdlib/lapack/base/zla_herpvgrw
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

var zla_herpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_herpvgrw = main;
} else {
	zla_herpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_herpvgrw;

// exports: { "ndarray": "zla_herpvgrw.ndarray" }
