/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Applies a vector of real plane rotations to elements of two real vectors.
*
* @param {NonNegativeInteger} N - number of plane rotations to apply
* @param {Float64Array} x - first input array
* @param {integer} strideX - `x` stride length
* @param {Float64Array} y - second input array
* @param {integer} strideY - `y` stride length
* @param {Float64Array} c - array of cosines of the plane rotations
* @param {Float64Array} s - array of sines of the plane rotations
* @param {integer} strideCS - stride length for `c` and `s`
* @returns {void}
*/
function dlartv( N, x, strideX, y, strideY, c, s, strideCS ) {
	var ox = stride2offset( N, strideX );
	var oy = stride2offset( N, strideY );
	var ocs = stride2offset( N, strideCS );
	base( N, x, strideX, ox, y, strideY, oy, c, strideCS, ocs, s, strideCS, ocs );
}


// EXPORTS //

module.exports = dlartv;
