

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Refine eigenvalue approximations using bisection given initial intervals.
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} E2 - input array
* @param {integer} strideE2 - stride length for `E2`
* @param {NonNegativeInteger} offsetE2 - starting index for `E2`
* @param {integer} ifirst - ifirst
* @param {integer} ilast - ilast
* @param {number} rtol - rtol
* @param {integer} offset - offset
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Float64Array} WERR - input array
* @param {integer} strideWERR - stride length for `WERR`
* @param {NonNegativeInteger} offsetWERR - starting index for `WERR`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @param {number} pivmin - pivmin
* @param {number} spdiam - spdiam
* @returns {integer} status code (0 = success)
*/
function dlarrj( N, d, strideD, offsetD, E2, strideE2, offsetE2, ifirst, ilast, rtol, offset, w, strideW, offsetW, WERR, strideWERR, offsetWERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, pivmin, spdiam ) { // eslint-disable-line max-len, max-params
	return base( N, d, strideD, offsetD, E2, strideE2, offsetE2, ifirst, ilast, rtol, offset, w, strideW, offsetW, WERR, strideWERR, offsetWERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, pivmin, spdiam ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlarrj;
