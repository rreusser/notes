

'use strict';

/**
* Back-transforms eigenvectors after balancing by dgebal
*
* @module @stdlib/lapack/base/dgebak
*
*
* @example
* var dgebak = require( '@stdlib/lapack/base/dgebak' );
*
* var N = 3;
* var V = discreteUniform( N * N, -10, 10, opts );
* var SCALE = discreteUniform( N, -10, 10, opts );
*
* dgebak.ndarray( 'both', 'left', N, N, N, SCALE, 1, 0, N, V, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgebak;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgebak = main;
} else {
	dgebak = tmp;
}


// EXPORTS //

module.exports = dgebak;

// exports: { "ndarray": "dgebak.ndarray" }
