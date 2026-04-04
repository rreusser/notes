

'use strict';

/**
* Solve complex Sylvester matrix equation
*
* @module @stdlib/lapack/base/ztrsyl
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

var ztrsyl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrsyl = main;
} else {
	ztrsyl = tmp;
}


// EXPORTS //

module.exports = ztrsyl;

// exports: { "ndarray": "ztrsyl.ndarray" }
