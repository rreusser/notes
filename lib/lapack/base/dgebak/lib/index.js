

'use strict';

/**
* Back-transforms eigenvectors after balancing by dgebal
*
* @module @stdlib/lapack/base/dgebak
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
