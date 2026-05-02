
'use strict';

/**
* Sum of absolute values of a complex vector.
*
* @module @stdlib/lapack/base/dzsum1
*
*
* @example
* var dzsum1 = require( '@stdlib/lapack/base/dzsum1' );
*
* var N = 3;
* var CX = discreteUniform( N, -10, 10, opts );
*
* dzsum1.ndarray( N, CX, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dzsum1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dzsum1 = main;
} else {
	dzsum1 = tmp;
}


// EXPORTS //

module.exports = dzsum1;

// exports: { "ndarray": "dzsum1.ndarray" }
