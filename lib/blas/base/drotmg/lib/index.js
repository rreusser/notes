
'use strict';

/**
* Construct a modified Givens plane rotation.
*
* @module @stdlib/blas/base/drotmg
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

var drotmg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	drotmg = main;
} else {
	drotmg = tmp;
}


// EXPORTS //

module.exports = drotmg;

// exports: { "ndarray": "drotmg.ndarray" }
