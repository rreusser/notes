

'use strict';

// MAIN //

/**
* Applies a plane rotation.
*
* @private
* @param {NonNegativeInteger} N - number of indexed elements
* @param {Float64Array} x - first input array
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - second input array
* @param {integer} strideY - `y` stride length
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {number} c - cosine of the angle of rotation
* @param {number} s - sine of the angle of rotation
* @returns {Float64Array} `y`
*/
function drot( N, x, strideX, offsetX, y, strideY, offsetY, c, s ) { // eslint-disable-line max-len, max-params
	var temp;
	var ix;
	var iy;
	var i;

	if ( N <= 0 ) {
		return y;
	}
	ix = offsetX;
	iy = offsetY;
	for ( i = 0; i < N; i++ ) {
		temp = ( c * x[ ix ] ) + ( s * y[ iy ] );
		y[ iy ] = ( c * y[ iy ] ) - ( s * x[ ix ] );
		x[ ix ] = temp;
		ix += strideX;
		iy += strideY;
	}
	return y;
}


// EXPORTS //

module.exports = drot;
