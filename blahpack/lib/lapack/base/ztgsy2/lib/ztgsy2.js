'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves the generalized Sylvester matrix equation for small subsystems.
*
* @param {string} trans - 'no-transpose' or 'conjugate-transpose'
* @param {integer} ijob - 0: solve only; 2: solve + estimate DIF
* @param {PositiveInteger} M - number of rows in C, F and order of (A,D)
* @param {PositiveInteger} N - number of columns in C, F and order of (B,E)
* @param {Complex128Array} A - M-by-M upper triangular matrix
* @param {PositiveInteger} LDA - leading dimension of A
* @param {Complex128Array} B - N-by-N upper triangular matrix
* @param {PositiveInteger} LDB - leading dimension of B
* @param {Complex128Array} C - M-by-N right-hand side / solution
* @param {PositiveInteger} LDC - leading dimension of C
* @param {Complex128Array} D - M-by-M upper triangular matrix
* @param {PositiveInteger} LDD - leading dimension of D
* @param {Complex128Array} E - N-by-N upper triangular matrix
* @param {PositiveInteger} LDE - leading dimension of E
* @param {Complex128Array} F - M-by-N right-hand side / solution
* @param {PositiveInteger} LDF - leading dimension of F
* @param {Float64Array} scale - output: scale[0] is the scaling factor
* @param {Float64Array} rdsum - in/out: rdsum[0]
* @param {Float64Array} rdscal - in/out: rdscal[0]
* @returns {integer} info - 0 if successful, >0 from zgetc2
*/
function ztgsy2( trans, ijob, M, N, A, LDA, B, LDB, C, LDC, D, LDD, E, LDE, F, LDF, scale, rdsum, rdscal ) { // eslint-disable-line max-len, max-params
	return base( trans, ijob, M, N, A, 1, LDA, 0, B, 1, LDB, 0, C, 1, LDC, 0, D, 1, LDD, 0, E, 1, LDE, 0, F, 1, LDF, 0, scale, rdsum, rdscal ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztgsy2;
