/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Applies a vector of real plane rotations from both sides to a sequence of 2-by-2 symmetric matrices.
*
* @param {NonNegativeInteger} N - number of plane rotations to apply
* @param {Float64Array} x - first input array
* @param {Float64Array} y - second input array
* @param {Float64Array} z - third input array
* @param {integer} strideXYZ - stride length for `x`, `y`, and `z`
* @param {Float64Array} c - array of cosines of the plane rotations
* @param {Float64Array} s - array of sines of the plane rotations
* @param {integer} strideCS - stride length for `c` and `s`
* @returns {void}
*/
function dlar2v( N, x, y, z, strideXYZ, c, s, strideCS ) {
	var oxyz = stride2offset( N, strideXYZ );
	var ocs = stride2offset( N, strideCS );
	base( N, x, strideXYZ, oxyz, y, strideXYZ, oxyz, z, strideXYZ, oxyz, c, strideCS, ocs, s, strideCS, ocs );
}


// EXPORTS //

module.exports = dlar2v;
