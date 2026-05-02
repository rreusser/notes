

'use strict';

/**
* Complex symmetric matrix norm
*
* @module @stdlib/lapack/base/zlansy
*
*
* @example
* var zlansy = require( '@stdlib/lapack/base/zlansy' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zlansy.ndarray( '1', 'upper', N, A, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlansy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlansy = main;
} else {
	zlansy = tmp;
}


// EXPORTS //

module.exports = zlansy;

// exports: { "ndarray": "zlansy.ndarray" }
