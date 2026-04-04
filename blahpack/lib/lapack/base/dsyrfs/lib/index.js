
'use strict';

/**
* Improves solution to a symmetric indefinite system and provides error bounds.
*
* @module @stdlib/lapack/base/dsyrfs
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

var dsyrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsyrfs = main;
} else {
	dsyrfs = tmp;
}


// EXPORTS //

module.exports = dsyrfs;

// exports: { "ndarray": "dsyrfs.ndarray" }
