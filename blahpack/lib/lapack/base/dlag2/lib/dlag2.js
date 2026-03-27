

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the eigenvalues of a 2-by-2 generalized eigenvalue problem `A - w B`, with scaling as necessary to avoid over-/underflow.
*
* @param {string} order - storage layout
* @param {Float64Array} A - input 2-by-2 matrix A
* @param {integer} LDA - leading dimension of A
* @param {Float64Array} B - input 2-by-2 upper triangular matrix B
* @param {integer} LDB - leading dimension of B
* @param {number} safmin - smallest positive number such that `1/safmin` does not overflow
* @returns {Object} object with fields: scale1, scale2, wr1, wr2, wi
*/
function dlag2( order, A, LDA, B, LDB, safmin ) {
	if ( order === 'row-major' ) {
		return base( A, LDA, 1, 0, B, LDB, 1, 0, safmin );
	}
	return base( A, 1, LDA, 0, B, 1, LDB, 0, safmin );
}


// EXPORTS //

module.exports = dlag2;
