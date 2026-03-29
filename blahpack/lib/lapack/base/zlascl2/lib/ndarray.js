

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform diagonal scaling on a complex matrix.
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - real scaling vector of length `M`
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Complex128Array} X - input/output complex matrix
* @param {integer} strideX1 - stride of the first dimension of `X` (in complex elements)
* @param {integer} strideX2 - stride of the second dimension of `X` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `X` (in complex elements)
* @returns {Complex128Array} `X`
*/
function zlascl2( M, N, d, strideD, offsetD, X, strideX1, strideX2, offsetX ) { // eslint-disable-line max-len, max-params
	return base( M, N, d, strideD, offsetD, X, strideX1, strideX2, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlascl2;
