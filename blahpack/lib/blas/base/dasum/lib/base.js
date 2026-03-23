'use strict';

// MAIN //

/**
* Computes the sum of absolute values of a double-precision floating-point vector.
*
* @private
* @param {NonNegativeInteger} N - number of indexed elements
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @returns {number} sum of absolute values
*/
function dasum( N, x, stride, offset ) {
	var dtemp;
	var ix;
	var m;
	var i;

	dtemp = 0.0;
	if ( N <= 0 || stride <= 0 ) {
		return dtemp;
	}
	ix = offset;

	// Use unrolled loops if stride is equal to 1...
	if ( stride === 1 ) {
		m = N % 6;
		if ( m > 0 ) {
			for ( i = 0; i < m; i++ ) {
				dtemp += Math.abs( x[ ix ] );
				ix += 1;
			}
		}
		if ( N < 6 ) {
			return dtemp;
		}
		for ( i = m; i < N; i += 6 ) {
			dtemp += Math.abs( x[ix] ) + Math.abs( x[ix+1] ) + Math.abs( x[ix+2] ) + Math.abs( x[ix+3] ) + Math.abs( x[ix+4] ) + Math.abs( x[ix+5] ); // eslint-disable-line max-len
			ix += 6;
		}
		return dtemp;
	}
	for ( i = 0; i < N; i++ ) {
		dtemp += Math.abs( x[ ix ] );
		ix += stride;
	}
	return dtemp;
}


// EXPORTS //

module.exports = dasum;
