

'use strict';

/**
* Complex symmetric indefinite expert solver
*
* @module @stdlib/lapack/base/zsysvx
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

var zsysvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsysvx = main;
} else {
	zsysvx = tmp;
}


// EXPORTS //

module.exports = zsysvx;

// exports: { "ndarray": "zsysvx.ndarray" }
