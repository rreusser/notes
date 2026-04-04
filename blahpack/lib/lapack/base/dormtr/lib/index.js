
'use strict';

/**
* Apply orthogonal matrix Q from dsytrd to a general matrix.
*
* @module @stdlib/lapack/base/dormtr
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

var dormtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormtr = main;
} else {
	dormtr = tmp;
}


// EXPORTS //

module.exports = dormtr;

// exports: { "ndarray": "dormtr.ndarray" }
