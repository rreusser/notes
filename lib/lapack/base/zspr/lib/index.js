

'use strict';

/**
* Perform the symmetric rank-1 update of a complex symmetric packed matrix
*
* @module @stdlib/lapack/base/zspr
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

var zspr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zspr = main;
} else {
	zspr = tmp;
}


// EXPORTS //

module.exports = zspr;

// exports: { "ndarray": "zspr.ndarray" }
