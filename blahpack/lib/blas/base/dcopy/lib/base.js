

'use strict';

// VARIABLES //

var M = 7;

// MAIN //

/**
* Copies a vector x to a vector y.
*
* @private
* @param {PositiveInteger} N - number of indexed elements
* @param {Float64Array} x - input array
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting `x` index
* @param {Float64Array} y - output array
* @param {integer} strideY - `y` stride length
* @param {NonNegativeInteger} offsetY - starting `y` index
* @returns {Float64Array} `y`
*/
function dcopy( N, x, strideX, offsetX, y, strideY, offsetY ) { // eslint-disable-line max-len, max-params
	var ix;
	var iy;
	var m;
	var i;

	if ( N <= 0 ) {
		return y;
	}
	ix = offsetX;
	iy = offsetY;

	// Use unrolled loops if both strides are equal to 1...
	if ( strideX === 1 && strideY === 1 ) {
		m = N % M;
		if ( m > 0 ) {
			for ( i = 0; i < m; i++ ) {
				y[ iy ] = x[ ix ];
				ix += 1;
				iy += 1;
			}
		}
		if ( N < M ) {
			return y;
		}
		for ( i = m; i < N; i += M ) {
			y[iy] = x[ix];
			y[iy+1] = x[ix+1];
			y[iy+2] = x[ix+2];
			y[iy+3] = x[ix+3];
			y[iy+4] = x[ix+4];
			y[iy+5] = x[ix+5];
			y[iy+6] = x[ix+6];
			ix += M;
			iy += M;
		}
		return y;
	}
	for ( i = 0; i < N; i++ ) {
		y[ iy ] = x[ ix ];
		ix += strideX;
		iy += strideY;
	}
	return y;
}


// EXPORTS //

module.exports = dcopy;
