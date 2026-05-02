

'use strict';

/**
* Set initial vector for Francis QR step
*
* @module @stdlib/lapack/base/zlaqr1
*
*
* @example
* var zlaqr1 = require( '@stdlib/lapack/base/zlaqr1' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var v = discreteUniform( N, -10, 10, opts );
*
* zlaqr1.ndarray( N, H, N, 1, 0, 1, 1, v, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqr1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr1 = main;
} else {
	zlaqr1 = tmp;
}


// EXPORTS //

module.exports = zlaqr1;

// exports: { "ndarray": "zlaqr1.ndarray" }
