

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Swaps adjacent diagonal blocks of a real upper quasi-triangular matrix
*
* @param {boolean} wantq - wantq
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} T - input matrix
* @param {integer} strideT1 - stride of the first dimension of `T`
* @param {integer} strideT2 - stride of the second dimension of `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @param {Float64Array} Q - input matrix
* @param {integer} strideQ1 - stride of the first dimension of `Q`
* @param {integer} strideQ2 - stride of the second dimension of `Q`
* @param {NonNegativeInteger} offsetQ - starting index for `Q`
* @param {integer} j1 - j1
* @param {integer} n1 - n1
* @param {integer} n2 - n2
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function dlaexc( wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, j1, n1, n2, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, j1, n1, n2, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaexc;
