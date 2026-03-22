

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the singular values of a 2-by-2 triangular matrix.
*
* @param {number} f - the (1,1) element
* @param {number} g - the (1,2) element
* @param {number} h - the (2,2) element
* @param {Float64Array} out - output array: out[0]=ssmin, out[1]=ssmax
* @returns {Float64Array} out
*/
function dlas2( f, g, h, out ) {
	return base( f, g, h, out );
}


// EXPORTS //

module.exports = dlas2;
