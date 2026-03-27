

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * Sets a scalar multiple of the first column of the product
 * (H - (sr1 + i*si1)*I)*(H - (sr2 + i*si2)*I), scaling to avoid
 * overflows and most underflows.
 *
 *
 * @param {NonNegativeInteger} N - order of the matrix H (must be 2 or 3)
 * @param {Float64Array} H - input matrix
 * @param {integer} strideH1 - stride of the first dimension of `H`
 * @param {integer} strideH2 - stride of the second dimension of `H`
 * @param {NonNegativeInteger} offsetH - starting index for `H`
 * @param {number} sr1 - real part of first shift
 * @param {number} si1 - imaginary part of first shift
 * @param {number} sr2 - real part of second shift
 * @param {number} si2 - imaginary part of second shift
 * @param {Float64Array} v - output array of length N
 * @param {integer} strideV - stride length for `v`
 * @param {NonNegativeInteger} offsetV - starting index for `v`
 */
function dlaqr1( N, H, strideH1, strideH2, offsetH, sr1, si1, sr2, si2, v, strideV, offsetV ) { // eslint-disable-line max-len, max-params
	return base( N, H, strideH1, strideH2, offsetH, sr1, si1, sr2, si2, v, strideV, offsetV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqr1;
