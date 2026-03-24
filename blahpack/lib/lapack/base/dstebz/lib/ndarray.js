

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection
*
* @param {string} range - specifies the operation type
* @param {string} order - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {number} vl - vl
* @param {number} vu - vu
* @param {integer} il - il
* @param {integer} iu - iu
* @param {number} abstol - abstol
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {NonNegativeInteger} M - number of rows
* @param {integer} nsplit - nsplit
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Int32Array} IBLOCK - input array
* @param {integer} strideIBLOCK - stride length for `IBLOCK`
* @param {NonNegativeInteger} offsetIBLOCK - starting index for `IBLOCK`
* @param {Int32Array} ISPLIT - input array
* @param {integer} strideISPLIT - stride length for `ISPLIT`
* @param {NonNegativeInteger} offsetISPLIT - starting index for `ISPLIT`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @returns {integer} status code (0 = success)
*/
function dstebz( range, order, N, vl, vu, il, iu, abstol, d, strideD, offsetD, e, strideE, offsetE, M, nsplit, w, strideW, offsetW, IBLOCK, strideIBLOCK, offsetIBLOCK, ISPLIT, strideISPLIT, offsetISPLIT, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	return base( range, order, N, vl, vu, il, iu, abstol, d, strideD, offsetD, e, strideE, offsetE, M, nsplit, w, strideW, offsetW, IBLOCK, strideIBLOCK, offsetIBLOCK, ISPLIT, strideISPLIT, offsetISPLIT, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dstebz;
