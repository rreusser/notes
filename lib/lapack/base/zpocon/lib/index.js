

'use strict';

/**
* Estimate the reciprocal condition number of a complex positive definite matrix
*
* @module @stdlib/lapack/base/zpocon
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

var zpocon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpocon = main;
} else {
	zpocon = tmp;
}


// EXPORTS //

module.exports = zpocon;

// exports: { "ndarray": "zpocon.ndarray" }
