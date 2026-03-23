'use strict';

// MAIN //

/**
* Performs one of the matrix-vector operations:
*   y := alpha*A*x + beta*y,   or   y := alpha*A^T*x + beta*y
*
* @private
* @param {string} trans - specifies whether A is transposed ('no-transpose', 'T', or 'C')
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {number} alpha - scalar multiplier for A*x
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - index offset for A
* @param {Float64Array} x - input vector
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting `x` index
* @param {number} beta - scalar multiplier for y
* @param {Float64Array} y - input/output vector
* @param {integer} strideY - `y` stride length
* @param {NonNegativeInteger} offsetY - starting `y` index
* @returns {Float64Array} `y`
*/
function dgemv( trans, M, N, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY ) {
	var temp;
	var lenx;
	var leny;
	var sa1;
	var sa2;
	var ia;
	var ix;
	var iy;
	var jx;
	var jy;
	var i;
	var j;

	sa1 = strideA1;
	sa2 = strideA2;

	if ( M === 0 || N === 0 || ( alpha === 0.0 && beta === 1.0 ) ) {
		return y;
	}

	var noTrans = ( trans === 'no-transpose' || trans === 'no-transpose' );
	if ( noTrans ) {
		lenx = N;
		leny = M;
	} else {
		lenx = M;
		leny = N;
	}

	// First form y := beta*y
	if ( beta !== 1.0 ) {
		iy = offsetY;
		if ( beta === 0.0 ) {
			for ( i = 0; i < leny; i++ ) {
				y[ iy ] = 0.0;
				iy += strideY;
			}
		} else {
			for ( i = 0; i < leny; i++ ) {
				y[ iy ] = beta * y[ iy ];
				iy += strideY;
			}
		}
	}
	if ( alpha === 0.0 ) {
		return y;
	}

	if ( noTrans ) {
		// Form y := alpha*A*x + y
		jx = offsetX;
		for ( j = 0; j < N; j++ ) {
			temp = alpha * x[ jx ];
			iy = offsetY;
			ia = offsetA + j * sa2;
			for ( i = 0; i < M; i++ ) {
				y[ iy ] += temp * A[ ia ];
				iy += strideY;
				ia += sa1;
			}
			jx += strideX;
		}
	} else {
		// Form y := alpha*A^T*x + y
		jy = offsetY;
		for ( j = 0; j < N; j++ ) {
			temp = 0.0;
			ix = offsetX;
			ia = offsetA + j * sa2;
			for ( i = 0; i < M; i++ ) {
				temp += A[ ia ] * x[ ix ];
				ix += strideX;
				ia += sa1;
			}
			y[ jy ] += alpha * temp;
			jy += strideY;
		}
	}
	return y;
}

// EXPORTS //

module.exports = dgemv;
