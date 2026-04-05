
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Counts the number of eigenvalues of a symmetric tridiagonal matrix in an interval.
*
* @param {string} jobt - specifies the matrix type: `'tridiagonal'` for matrix T, `'ldl'` for matrix `L * D * L^T`
* @param {NonNegativeInteger} N - order of the matrix
* @param {number} vl - lower bound for the eigenvalues
* @param {number} vu - upper bound for the eigenvalues
* @param {Float64Array} D - diagonal elements (length N)
* @param {Float64Array} E - off-diagonal elements (length N-1)
* @param {number} pivmin - minimum pivot in the Sturm sequence
* @returns {Object} object with `info`, `eigcnt`, `lcnt`, and `rcnt` properties
*/
function dlarrc( jobt, N, vl, vu, D, E, pivmin ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( jobt, N, vl, vu, D, 1, 0, E, 1, 0, pivmin );
}


// EXPORTS //

module.exports = dlarrc;
