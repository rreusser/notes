
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} ZX - ZX
* @param {integer} strideZX - strideZX
* @returns {integer} 0-based index of the element with maximum absolute value
*/
function izmax1( N, ZX, strideZX ) {
	var ozx;

	ozx = stride2offset( N, strideZX );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, ZX, strideZX, ozx );
}


// EXPORTS //

module.exports = izmax1;
