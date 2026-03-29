

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Applies a vector of real plane rotations from both sides to a sequence of 2-by-2 symmetric matrices.
*
* @param {NonNegativeInteger} N - number of plane rotations to apply
* @param {Float64Array} x - first input array
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - second input array
* @param {integer} strideY - `y` stride length
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {Float64Array} z - third input array
* @param {integer} strideZ - `z` stride length
* @param {NonNegativeInteger} offsetZ - starting index for `z`
* @param {Float64Array} c - array of cosines of the plane rotations
* @param {integer} strideC - `c` stride length
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {Float64Array} s - array of sines of the plane rotations
* @param {integer} strideS - `s` stride length
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @returns {void}
*/
function dlar2v( N, x, strideX, offsetX, y, strideY, offsetY, z, strideZ, offsetZ, c, strideC, offsetC, s, strideS, offsetS ) { // eslint-disable-line max-len, max-params
	return base( N, x, strideX, offsetX, y, strideY, offsetY, z, strideZ, offsetZ, c, strideC, offsetC, s, strideS, offsetS ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlar2v;
