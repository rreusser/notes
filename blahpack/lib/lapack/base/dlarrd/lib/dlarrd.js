

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes eigenvalues of a symmetric tridiagonal matrix to suitable accuracy
*
* @param {string} range - specifies the operation type
* @param {string} order - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {number} vl - vl
* @param {number} vu - vu
* @param {integer} il - il
* @param {integer} iu - iu
* @param {Float64Array} GERS - input array
* @param {integer} strideGERS - stride length for `GERS`
* @param {number} reltol - reltol
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {Float64Array} E2 - input array
* @param {integer} strideE2 - stride length for `E2`
* @param {number} pivmin - pivmin
* @param {integer} nsplit - nsplit
* @param {Int32Array} ISPLIT - input array
* @param {integer} strideISPLIT - stride length for `ISPLIT`
* @param {NonNegativeInteger} offsetISPLIT - starting index for `ISPLIT`
* @param {NonNegativeInteger} M - number of rows
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {Float64Array} WERR - input array
* @param {integer} strideWERR - stride length for `WERR`
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
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function dlarrd( range, order, N, vl, vu, il, iu, GERS, strideGERS, reltol, d, strideD, e, strideE, E2, strideE2, pivmin, nsplit, ISPLIT, strideISPLIT, offsetISPLIT, M, w, strideW, WERR, strideWERR, wl, wu, IBLOCK, strideIBLOCK, offsetIBLOCK, INDEXW, strideINDEXW, offsetINDEXW, WORK, strideWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. 22th argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	return base( range, order, N, vl, vu, il, iu, GERS, strideGERS, 0, reltol, d, strideD, 0, e, strideE, 0, E2, strideE2, 0, pivmin, nsplit, ISPLIT, strideISPLIT, offsetISPLIT, M, w, strideW, 0, WERR, strideWERR, 0, wl, wu, IBLOCK, strideIBLOCK, offsetIBLOCK, INDEXW, strideINDEXW, offsetINDEXW, WORK, strideWORK, 0, IWORK, strideIWORK, offsetIWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlarrd;
