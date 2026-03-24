

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Reorder Schur factorization of a complex matrix
*
* @param {string} compq - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} T - input matrix
* @param {integer} strideT1 - stride of the first dimension of `T`
* @param {integer} strideT2 - stride of the second dimension of `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @param {Float64Array} Q - output matrix
* @param {integer} strideQ1 - stride of the first dimension of `Q`
* @param {integer} strideQ2 - stride of the second dimension of `Q`
* @param {NonNegativeInteger} offsetQ - starting index for `Q`
* @param {integer} ifst - ifst
* @param {integer} ilst - ilst
* @returns {integer} status code (0 = success)
*/
function ztrexc( compq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, ifst, ilst ) { // eslint-disable-line max-len, max-params
	return base( compq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, ifst, ilst ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrexc;
