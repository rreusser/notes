
'use strict';

/**
* Perform symmetric rank-1 update of a packed matrix.
*
* @module @stdlib/blas/base/dspr
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

var dspr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspr = main;
} else {
	dspr = tmp;
}


// EXPORTS //

module.exports = dspr;

// exports: { "ndarray": "dspr.ndarray" }
