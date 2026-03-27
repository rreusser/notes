

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * Takes the sum of the absolute values of a complex vector and returns a
 * double precision result.
 *
 * Based on DZASUM from Level 1 BLAS. The change is to use the 'genuine'
 * absolute value (cabs) rather than dcabs1.
 *
 *
 * @param {NonNegativeInteger} N - number of complex elements
 * @param {Complex128Array} CX - complex input vector
 * @param {integer} strideCX - stride for `CX` (in complex elements)
 * @param {NonNegativeInteger} offsetCX - starting index for `CX` (in complex elements)
 * @returns {number} sum of absolute values
 */
function dzsum1( N, x, stride, offset, incx ) {
	return base( N, x, stride, offset, incx );
}


// EXPORTS //

module.exports = dzsum1;
