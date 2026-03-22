

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Scale a complex vector and add to another complex vector: y := alpha*x + y.
*
* @param {PositiveInteger} N - number of complex elements
* @param {Complex128} za - complex scalar
* @param {Complex128Array} zx - input vector
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx` (in complex elements)
* @param {Complex128Array} zy - input/output vector
* @param {integer} strideY - stride for `zy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `zy` (in complex elements)
* @returns {Complex128Array} `zy`
*/
function zaxpy( N, za, zx, strideX, offsetX, zy, strideY, offsetY ) { // eslint-disable-line max-len, max-params
	return base( N, za, zx, strideX, offsetX, zy, strideY, offsetY ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zaxpy;
