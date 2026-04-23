
'use strict';

/**
* Estimate the reciprocal condition number of a Hermitian indefinite matrix.
*
* @module @stdlib/lapack/base/zhecon
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

var zhecon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhecon = main;
} else {
	zhecon = tmp;
}


// EXPORTS //

module.exports = zhecon;

// exports: { "ndarray": "zhecon.ndarray" }
