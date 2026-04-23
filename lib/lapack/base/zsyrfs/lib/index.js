

'use strict';

/**
* Complex symmetric iterative refinement
*
* @module @stdlib/lapack/base/zsyrfs
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

var zsyrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsyrfs = main;
} else {
	zsyrfs = tmp;
}


// EXPORTS //

module.exports = zsyrfs;

// exports: { "ndarray": "zsyrfs.ndarray" }
