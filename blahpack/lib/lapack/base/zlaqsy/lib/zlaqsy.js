
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Equilibrates a symmetric matrix A using the scaling factors in the vector S.
*
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} A - input/output N-by-N symmetric matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} s - scaling factors, length N
* @param {integer} strideS - stride for s
* @param {number} scond - ratio of smallest to largest scaling factor
* @param {number} amax - absolute value of largest matrix element
* @returns {string} equed - 'none' or 'yes'
*/
function zlaqsy( uplo, N, A, LDA, s, strideS, scond, amax ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var os;

	sa1 = 1;
	sa2 = LDA;
	os = stride2offset( N, strideS );
	return base( uplo, N, A, sa1, sa2, 0, s, strideS, os, scond, amax );
}


// EXPORTS //

module.exports = zlaqsy;
