
'use strict';

/**
* Scale a complex vector by the reciprocal of a real scalar with overflow protection.
*
* @module @stdlib/lapack/base/zdrscl
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

var zdrscl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zdrscl = main;
} else {
	zdrscl = tmp;
}


// EXPORTS //

module.exports = zdrscl;

// exports: { "ndarray": "zdrscl.ndarray" }
