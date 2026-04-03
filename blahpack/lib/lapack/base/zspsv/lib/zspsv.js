
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the solution to a complex system of linear equations A * X = B where A is symmetric in packed storage.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Complex128Array} AP - packed symmetric matrix
* @param {Int32Array} IPIV - pivot index output array
* @param {integer} strideIPIV - stride for IPIV
* @param {Complex128Array} B - right-hand side matrix
* @param {PositiveInteger} LDB - leading dimension of B
* @returns {integer} info - status code
*/
function zspsv( uplo, N, nrhs, AP, IPIV, strideIPIV, B, LDB ) { // eslint-disable-line max-len, max-params
	var oipiv;

	oipiv = stride2offset( N, strideIPIV );
	return base( uplo, N, nrhs, AP, 1, 0, IPIV, strideIPIV, oipiv, B, 1, LDB, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zspsv;
