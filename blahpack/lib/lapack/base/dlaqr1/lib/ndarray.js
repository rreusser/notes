

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Sets a scalar multiple of the first column of H - shift product
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} H - input matrix
* @param {integer} strideH1 - stride of the first dimension of `H`
* @param {integer} strideH2 - stride of the second dimension of `H`
* @param {NonNegativeInteger} offsetH - starting index for `H`
* @param {number} sr1 - sr1
* @param {number} si1 - si1
* @param {number} sr2 - sr2
* @param {number} si2 - si2
* @param {Float64Array} v - output array
* @param {integer} strideV - stride length for `v`
* @param {NonNegativeInteger} offsetV - starting index for `v`
*/
function dlaqr1( N, H, strideH1, strideH2, offsetH, sr1, si1, sr2, si2, v, strideV, offsetV ) { // eslint-disable-line max-len, max-params
	return base( N, H, strideH1, strideH2, offsetH, sr1, si1, sr2, si2, v, strideV, offsetV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqr1;
