
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the generalized eigenvalues, the generalized complex Schur form.
* (S,T), and optionally the left and/or right matrices of Schur vectors for
* a pair of N-by-N complex nonsymmetric matrices (A,B).
*
* @param {string} jobvsl - `'compute-vectors'` or `'no-vectors'`
* @param {string} jobvsr - `'compute-vectors'` or `'no-vectors'`
* @param {string} sort - `'sorted'` or `'not-sorted'`
* @param {Function} selctg - selection function `(alphaRe, alphaIm, betaRe, betaIm) => boolean`
* @param {NonNegativeInteger} N - order of matrices A and B
* @param {Complex128Array} A - input matrix A (N x N), overwritten on exit
* @param {integer} strideA1 - first dimension stride of A (complex elements)
* @param {integer} strideA2 - second dimension stride of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @param {Complex128Array} B - input matrix B (N x N), overwritten on exit
* @param {integer} strideB1 - first dimension stride of B (complex elements)
* @param {integer} strideB2 - second dimension stride of B (complex elements)
* @param {NonNegativeInteger} offsetB - starting index for B (complex elements)
* @param {Complex128Array} ALPHA - output: eigenvalue numerators (length N)
* @param {integer} strideALPHA - stride for ALPHA (complex elements)
* @param {NonNegativeInteger} offsetALPHA - offset for ALPHA (complex elements)
* @param {Complex128Array} BETA - output: eigenvalue denominators (length N)
* @param {integer} strideBETA - stride for BETA (complex elements)
* @param {NonNegativeInteger} offsetBETA - offset for BETA (complex elements)
* @param {Complex128Array} VSL - output: left Schur vectors (N x N)
* @param {integer} strideVSL1 - first dimension stride of VSL (complex elements)
* @param {integer} strideVSL2 - second dimension stride of VSL (complex elements)
* @param {NonNegativeInteger} offsetVSL - starting index for VSL (complex elements)
* @param {Complex128Array} VSR - output: right Schur vectors (N x N)
* @param {integer} strideVSR1 - first dimension stride of VSR (complex elements)
* @param {integer} strideVSR2 - second dimension stride of VSR (complex elements)
* @param {NonNegativeInteger} offsetVSR - starting index for VSR (complex elements)
* @returns {Object} result with properties: info (integer status code), sdim (number of sorted eigenvalues)
*/
function zgges( jobvsl, jobvsr, sort, selctg, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, VSL, strideVSL1, strideVSL2, offsetVSL, VSR, strideVSR1, strideVSR2, offsetVSR ) { // eslint-disable-line max-len, max-params
	return base( jobvsl, jobvsr, sort, selctg, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, VSL, strideVSL1, strideVSL2, offsetVSL, VSR, strideVSR1, strideVSR2, offsetVSR ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgges;
