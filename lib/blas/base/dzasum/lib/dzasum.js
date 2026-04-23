
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} zx - zx
* @param {integer} strideX - strideX
* @param {NonNegativeInteger} offsetX - offsetX
* @returns {*} result
*/
function dzasum( N, zx, strideX, offsetX ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, zx, strideX, offsetX );
}


// EXPORTS //

module.exports = dzasum;
