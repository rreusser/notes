/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Applies a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices.
*
* @param {NonNegativeInteger} N - number of plane rotations to apply
* @param {Complex128Array} x - first input array (elements assumed real)
* @param {Complex128Array} y - second input array (elements assumed real)
* @param {Complex128Array} z - third input array (complex)
* @param {integer} strideXYZ - stride length for `x`, `y`, and `z` (in complex elements)
* @param {Float64Array} c - array of cosines of the plane rotations (real)
* @param {Complex128Array} s - array of sines of the plane rotations (complex)
* @param {integer} strideCS - stride length for `c` and `s`
* @returns {void}
*/
function zlar2v( N, x, y, z, strideXYZ, c, s, strideCS ) {
	var oxyz = stride2offset( N, strideXYZ );
	var ocs = stride2offset( N, strideCS );
	base( N, x, strideXYZ, oxyz, y, strideXYZ, oxyz, z, strideXYZ, oxyz, c, strideCS, ocs, s, strideCS, ocs );
}


// EXPORTS //

module.exports = zlar2v;
