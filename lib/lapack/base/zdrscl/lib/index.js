
'use strict';

/**
* Scale a complex vector by the reciprocal of a real scalar with overflow protection.
*
* @module @stdlib/lapack/base/zdrscl
*
*
* @example
* var zdrscl = require( '@stdlib/lapack/base/zdrscl' );
*
* var N = 3;
* var x = discreteUniform( N, -10, 10, opts );
*
* zdrscl.ndarray( N, 1, x, 1, 0 );
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
