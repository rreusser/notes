/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Applies a vector of complex plane rotations with real cosines to elements of two complex vectors.
*
* @param {NonNegativeInteger} N - number of plane rotations to apply
* @param {Complex128Array} x - first input array
* @param {integer} strideX - `x` stride length
* @param {Complex128Array} y - second input array
* @param {integer} strideY - `y` stride length
* @param {Float64Array} c - array of cosines of the plane rotations
* @param {Complex128Array} s - array of complex sines of the plane rotations
* @param {integer} strideCS - stride length for `c` and `s`
* @returns {void}
*/
function zlartv( N, x, strideX, y, strideY, c, s, strideCS ) {
	var ox = stride2offset( N, strideX );
	var oy = stride2offset( N, strideY );
	var ocs = stride2offset( N, strideCS );
	base( N, x, strideX, ox, y, strideY, oy, c, strideCS, ocs, s, strideCS, ocs );
}


// EXPORTS //

module.exports = zlartv;
