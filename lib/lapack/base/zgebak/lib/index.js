

'use strict';

/**
* Back-transforms eigenvectors after balancing by zgebal
*
* @module @stdlib/lapack/base/zgebak
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
