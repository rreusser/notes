

'use strict';

/**
* Generate unitary matrix Q from zhetrd
*
* @module @stdlib/lapack/base/zungtr
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

var zungtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zungtr = main;
} else {
	zungtr = tmp;
}


// EXPORTS //

module.exports = zungtr;

// exports: { "ndarray": "zungtr.ndarray" }
