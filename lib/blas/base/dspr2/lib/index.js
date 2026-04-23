
'use strict';

/**
* Perform symmetric rank-2 update of a packed matrix.
*
* @module @stdlib/blas/base/dspr2
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

var dspr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspr2 = main;
} else {
	dspr2 = tmp;
}


// EXPORTS //

module.exports = dspr2;

// exports: { "ndarray": "dspr2.ndarray" }
