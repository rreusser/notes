

'use strict';

/**
* Back-transforms eigenvectors after balancing by zgebal
*
* @module @stdlib/lapack/base/zgebak
*
*
* @example
* var zgebak = require( '@stdlib/lapack/base/zgebak' );
*
* var N = 3;
* var V = discreteUniform( N * N, -10, 10, opts );
* var SCALE = discreteUniform( N, -10, 10, opts );
*
* zgebak.ndarray( 'both', 'left', N, N, N, SCALE, 1, 0, N, V, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgebak;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgebak = main;
} else {
	zgebak = tmp;
}


// EXPORTS //

module.exports = zgebak;

// exports: { "ndarray": "zgebak.ndarray" }
