

'use strict';

/**
* Generate unitary matrix Q from QL reflectors
*
* @module @stdlib/lapack/base/zung2l
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

var zung2l;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zung2l = main;
} else {
	zung2l = tmp;
}


// EXPORTS //

module.exports = zung2l;

// exports: { "ndarray": "zung2l.ndarray" }
