

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute SVD of a 2-by-2 triangular matrix
*
* @param {number} f - f
* @param {number} g - g
* @param {number} h - h
* @param {number} ssmin - ssmin
* @param {number} ssmax - ssmax
* @param {number} snr - snr
* @param {number} csr - csr
* @param {number} snl - snl
* @param {number} csl - csl
*/
function dlasv2( f, g, h, ssmin, ssmax, snr, csr, snl, csl ) { // eslint-disable-line max-len, max-params
	return base( f, g, h, ssmin, ssmax, snr, csr, snl, csl ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlasv2;
