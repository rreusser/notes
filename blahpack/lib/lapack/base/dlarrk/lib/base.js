

'use strict';

// MAIN //

/**
* Computes one eigenvalue of a symmetric tridiagonal matrix to suitable accuracy
*
* @private
* @param {NonNegativeInteger} N - number of columns
* @param {integer} iw - iw
* @param {number} gl - gl
* @param {number} gu - gu
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} E2 - output array
* @param {integer} strideE2 - stride length for `E2`
* @param {NonNegativeInteger} offsetE2 - starting index for `E2`
* @param {number} pivmin - pivmin
* @param {number} reltol - reltol
* @param {number} w - w
* @param {number} werr - werr
* @returns {integer} status code (0 = success)
*/
function dlarrk( N, iw, gl, gu, d, strideD, offsetD, E2, strideE2, offsetE2, pivmin, reltol, w, werr ) { // eslint-disable-line max-len, max-params
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = dlarrk;
