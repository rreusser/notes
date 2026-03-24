

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Auxiliary bisection routine for tridiagonal eigenvalue computation
*
* @param {integer} ijob - ijob
* @param {integer} nitmax - nitmax
* @param {NonNegativeInteger} N - number of columns
* @param {integer} minp - minp
* @param {integer} nbmin - nbmin
* @param {number} abstol - abstol
* @param {number} reltol - reltol
* @param {number} pivmin - pivmin
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} E2 - input array
* @param {integer} strideE2 - stride length for `E2`
* @param {NonNegativeInteger} offsetE2 - starting index for `E2`
* @param {Int32Array} NVAL - input array
* @param {integer} strideNVAL - stride length for `NVAL`
* @param {NonNegativeInteger} offsetNVAL - starting index for `NVAL`
* @param {Float64Array} AB - input matrix
* @param {integer} strideAB1 - stride of the first dimension of `AB`
* @param {integer} strideAB2 - stride of the second dimension of `AB`
* @param {NonNegativeInteger} offsetAB - starting index for `AB`
* @param {Float64Array} c - input array
* @param {integer} strideC - stride length for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {integer} mout - mout
* @param {Int32Array} NAB - input matrix
* @param {integer} strideNAB1 - stride of the first dimension of `NAB`
* @param {integer} strideNAB2 - stride of the second dimension of `NAB`
* @param {NonNegativeInteger} offsetNAB - starting index for `NAB`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @returns {integer} status code (0 = success)
*/
function dlaebz( ijob, nitmax, N, mmax, minp, nbmin, abstol, reltol, pivmin, d, strideD, offsetD, e, strideE, offsetE, E2, strideE2, offsetE2, NVAL, strideNVAL, offsetNVAL, AB, strideAB1, strideAB2, offsetAB, c, strideC, offsetC, mout, NAB, strideNAB1, strideNAB2, offsetNAB, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	return base( ijob, nitmax, N, mmax, minp, nbmin, abstol, reltol, pivmin, d, strideD, offsetD, e, strideE, offsetE, E2, strideE2, offsetE2, NVAL, strideNVAL, offsetNVAL, AB, strideAB1, strideAB2, offsetAB, c, strideC, offsetC, mout, NAB, strideNAB1, strideNAB2, offsetNAB, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaebz;
