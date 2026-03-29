/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Performs a matrix-matrix product of the form `B := alpha*A*X + beta*B` where A is a complex tridiagonal matrix.
*
* @param {string} trans - specifies the operation: `'no-transpose'`, `'transpose'`, or `'conjugate-transpose'`
* @param {NonNegativeInteger} N - order of the tridiagonal matrix A
* @param {NonNegativeInteger} nrhs - number of right hand sides (columns of X and B)
* @param {number} alpha - real scalar multiplier (must be 0.0, 1.0, or -1.0)
* @param {Complex128Array} DL - sub-diagonal of A (length N-1)
* @param {integer} strideDL - stride for DL (in complex elements)
* @param {Complex128Array} d - diagonal of A (length N)
* @param {integer} strideD - stride for d (in complex elements)
* @param {Complex128Array} DU - super-diagonal of A (length N-1)
* @param {integer} strideDU - stride for DU (in complex elements)
* @param {Complex128Array} X - input matrix (N x NRHS)
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {number} beta - real scalar multiplier for B (0.0, 1.0, or -1.0)
* @param {Complex128Array} B - input/output matrix (N x NRHS)
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {void}
*/
function zlagtm( trans, N, nrhs, alpha, DL, strideDL, d, strideD, DU, strideDU, X, LDX, beta, B, LDB ) {
	var odl;
	var odu;
	var sb1;
	var sb2;
	var sx1;
	var sx2;
	var od;

	sx1 = 1;
	sx2 = LDX;
	sb1 = 1;
	sb2 = LDB;
	odl = stride2offset( N, strideDL );
	od = stride2offset( N, strideD );
	odu = stride2offset( N, strideDU );
	base( trans, N, nrhs, alpha, DL, strideDL, odl, d, strideD, od, DU, strideDU, odu, X, sx1, sx2, 0, beta, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zlagtm;
