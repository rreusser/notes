
'use strict';

/**
* Complex blocked RQ factorization.
*
* @module @stdlib/lapack/base/zgerqf
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

var zgerqf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgerqf = main;
} else {
	zgerqf = tmp;
}


// EXPORTS //

module.exports = zgerqf;

// exports: { "ndarray": "zgerqf.ndarray" }
