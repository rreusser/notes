

'use strict';

// MAIN //

/**
* Computes eigenvalues of a symmetric tridiagonal matrix to suitable accuracy
*
* @private
* @param {string} range - specifies the operation type
* @param {string} order - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {number} vl - vl
* @param {number} vu - vu
* @param {integer} il - il
* @param {integer} iu - iu
* @param {Float64Array} GERS - input array
* @param {integer} strideGERS - stride length for `GERS`
* @param {NonNegativeInteger} offsetGERS - starting index for `GERS`
* @param {number} reltol - reltol
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} E2 - input array
* @param {integer} strideE2 - stride length for `E2`
* @param {NonNegativeInteger} offsetE2 - starting index for `E2`
* @param {number} pivmin - pivmin
* @param {integer} nsplit - nsplit
* @param {Int32Array} ISPLIT - input array
* @param {integer} strideISPLIT - stride length for `ISPLIT`
* @param {NonNegativeInteger} offsetISPLIT - starting index for `ISPLIT`
* @param {NonNegativeInteger} M - number of rows
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Float64Array} WERR - input array
* @param {integer} strideWERR - stride length for `WERR`
* @param {NonNegativeInteger} offsetWERR - starting index for `WERR`
* @param {number} wl - wl
* @param {number} wu - wu
* @param {Int32Array} IBLOCK - input array
* @param {integer} strideIBLOCK - stride length for `IBLOCK`
* @param {NonNegativeInteger} offsetIBLOCK - starting index for `IBLOCK`
* @param {Int32Array} INDEXW - input array
* @param {integer} strideINDEXW - stride length for `INDEXW`
* @param {NonNegativeInteger} offsetINDEXW - starting index for `INDEXW`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @returns {integer} status code (0 = success)
*/
function dlarrd( range, order, N, vl, vu, il, iu, GERS, strideGERS, offsetGERS, reltol, d, strideD, offsetD, e, strideE, offsetE, E2, strideE2, offsetE2, pivmin, nsplit, ISPLIT, strideISPLIT, offsetISPLIT, M, w, strideW, offsetW, WERR, strideWERR, offsetWERR, wl, wu, IBLOCK, strideIBLOCK, offsetIBLOCK, INDEXW, strideINDEXW, offsetINDEXW, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = dlarrd;
