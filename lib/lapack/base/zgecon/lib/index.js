
'use strict';

/**
* Estimate the reciprocal condition number of a complex general matrix.
*
* @module @stdlib/lapack/base/zgecon
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

var zgecon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgecon = main;
} else {
	zgecon = tmp;
}


// EXPORTS //

module.exports = zgecon;

// exports: { "ndarray": "zgecon.ndarray" }
