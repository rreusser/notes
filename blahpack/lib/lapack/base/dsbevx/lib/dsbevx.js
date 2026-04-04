
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes selected eigenvalues and, optionally, eigenvectors of a real symmetric band matrix A.
*
* @param {string} jobz - `'no-vectors'` or `'compute-vectors'`
* @param {string} range - `'all'`, `'value'`, or `'index'`
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} kd - number of super- (or sub-) diagonals
* @param {Float64Array} AB - band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} Q - output orthogonal matrix (N-by-N)
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {number} vl - lower bound of eigenvalue interval
* @param {number} vu - upper bound of eigenvalue interval
* @param {integer} il - index of smallest eigenvalue (1-based)
* @param {integer} iu - index of largest eigenvalue (1-based)
* @param {number} abstol - absolute tolerance
* @param {Object} out - output object; out.M set to number of eigenvalues found
* @param {Float64Array} w - output eigenvalues
* @param {integer} strideW - stride for `w`
* @param {Float64Array} Z - output eigenvectors
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Float64Array} WORK - workspace
* @param {integer} strideWORK - stride for `WORK`
* @param {Int32Array} IWORK - integer workspace
* @param {integer} strideIWORK - stride for `IWORK`
* @param {Int32Array} IFAIL - output failure indices
* @param {integer} strideIFAIL - stride for `IFAIL`
* @returns {integer} info - 0 if successful
*/
function dsbevx( jobz, range, uplo, N, kd, AB, LDAB, Q, LDQ, vl, vu, il, iu, abstol, out, w, strideW, Z, LDZ, WORK, strideWORK, IWORK, strideIWORK, IFAIL, strideIFAIL ) { // eslint-disable-line max-len, max-params
	var oifail;
	var oiwork;
	var owork;
	var ow;

	ow = stride2offset( N, strideW );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	oifail = stride2offset( N, strideIFAIL );
	return base( jobz, range, uplo, N, kd, AB, 1, LDAB, 0, Q, 1, LDQ, 0, vl, vu, il, iu, abstol, out, w, strideW, ow, Z, 1, LDZ, 0, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork, IFAIL, strideIFAIL, oifail ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsbevx;
