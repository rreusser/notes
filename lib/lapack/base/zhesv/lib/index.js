

'use strict';

/**
* Complex Hermitian indefinite linear system solver
*
* @module @stdlib/lapack/base/zhesv
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

var zhesv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhesv = main;
} else {
	zhesv = tmp;
}


// EXPORTS //

module.exports = zhesv;

// exports: { "ndarray": "zhesv.ndarray" }
