
'use strict';

/**
* Find index of first element of maximum absolute value.
*
* @module @stdlib/lapack/base/izmax1
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

var izmax1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	izmax1 = main;
} else {
	izmax1 = tmp;
}


// EXPORTS //

module.exports = izmax1;

// exports: { "ndarray": "izmax1.ndarray" }
