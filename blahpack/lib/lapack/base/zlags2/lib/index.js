
'use strict';

/**
* Computes 2-by-2 unitary matrices U, V, and Q for the generalized upper (lower) triangular form.
*
* @module @stdlib/lapack/base/zlags2
*
* @example
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var zlags2 = require( '@stdlib/lapack/base/zlags2' );
*
* var out = zlags2( true, 4.0, new Complex128( 2.0, 1.0 ), 3.0, 1.0, new Complex128( 0.5, 0.25 ), 2.0 );
* // returns { csu: ~1.0, snuR: ~0.0, snuI: ~0.0, ... }
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlags2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlags2 = main;
} else {
	zlags2 = tmp;
}


// EXPORTS //

module.exports = zlags2;

// exports: { "ndarray": "zlags2.ndarray" }
