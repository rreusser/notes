
'use strict';

/**
* Estimates the reciprocal condition number of a symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/dsycon
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

var dsycon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsycon = main;
} else {
	dsycon = tmp;
}


// EXPORTS //

module.exports = dsycon;

// exports: { "ndarray": "dsycon.ndarray" }
