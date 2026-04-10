

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Pre-processor for dgesvj performing Jacobi rotations
*
* @param {number} jobv - jobv
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} sva - input array
* @param {integer} strideSVA - stride length for `sva`
* @param {NonNegativeInteger} offsetSVA - starting index for `sva`
* @param {integer} mv - mv
* @param {Float64Array} V - output matrix
* @param {integer} strideV1 - stride of the first dimension of `V`
* @param {integer} strideV2 - stride of the second dimension of `V`
* @param {NonNegativeInteger} offsetV - starting index for `V`
* @param {number} eps - eps
* @param {number} sfmin - sfmin
* @param {number} tol - tol
* @param {integer} nsweep - nsweep
* @param {Float64Array} work - input array
* @param {integer} strideWORK - stride length for `work`
* @param {NonNegativeInteger} offsetWORK - starting index for `work`
* @param {integer} lwork - lwork
* @returns {integer} status code (0 = success)
*/
function dgsvj0( jobv, M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, sva, strideSVA, offsetSVA, mv, V, strideV1, strideV2, offsetV, eps, sfmin, tol, nsweep, work, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( jobv, M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, sva, strideSVA, offsetSVA, mv, V, strideV1, strideV2, offsetV, eps, sfmin, tol, nsweep, work, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgsvj0;
