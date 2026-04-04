
'use strict';

// MODULES //

var Complex128Array = require( '@stdlib/array/complex128' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a complex Hermitian matrix using the factorization computed by zhetrf.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - Hermitian matrix (overwritten with inverse)
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - pivot indices from zhetrf
* @returns {integer} status code (0 = success)
*/
function zhetri( uplo, N, A, LDA, IPIV ) {
	var WORK = new Complex128Array( N );
	return base( uplo, N, A, 1, LDA, 0, IPIV, 1, 0, WORK, 1, 0 );
}


// EXPORTS //

module.exports = zhetri;
