

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Applies a real plane rotation to a pair of complex double-precision vectors.
*
* @param {NonNegativeInteger} N - number of complex elements
* @param {Float64Array} zx - first input/output array (interleaved complex)
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx`
* @param {Float64Array} zy - second input/output array (interleaved complex)
* @param {integer} strideY - stride for `zy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `zy`
* @param {number} c - cosine of rotation (real)
* @param {number} s - sine of rotation (real)
* @returns {Float64Array} `zx`
*/
function zdrot( N, zx, strideX, offsetX, zy, strideY, offsetY, c, s ) {
	return base( N, zx, strideX, offsetX, zy, strideY, offsetY, c, s );
}


// EXPORTS //

module.exports = zdrot;
