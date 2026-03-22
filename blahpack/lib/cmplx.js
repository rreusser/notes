/**
* @license Apache-2.0
*
* Complex number arithmetic on flat 2-element arrays.
*
* Convention: every complex number is a 2-element array-like [real, imag].
* Functions never allocate. Caller provides `out` for results.
*
* Usage:
*   var cmplx = require('./cmplx.js');
*   var z = new Float64Array(2);
*   var a = new Float64Array([3, 4]);
*   var b = new Float64Array([1, 2]);
*   cmplx.mul(z, a, b);  // z = a * b = [-5, 10]
*
* For BLAS complex vectors (interleaved real/imag), use .subarray()
* to get zero-copy views into the element at index k:
*   cmplx.mul(z, x.subarray(2*k, 2*k+2), y.subarray(2*j, 2*j+2));
*/

'use strict';

// MAIN //

/**
* Set a complex number from real and imaginary parts.
*
* @param {Float64Array} out - output complex number
* @param {number} re - real part
* @param {number} im - imaginary part
* @returns {Float64Array} out
*/
function cset( out, re, im ) {
	out[ 0 ] = re;
	out[ 1 ] = im;
	return out;
}

/**
* Return the real part of a complex number.
*
* @param {Float64Array} a - complex number
* @returns {number} real part
*/
function creal( a ) {
	return a[ 0 ];
}

/**
* Return the imaginary part of a complex number.
*
* @param {Float64Array} a - complex number
* @returns {number} imaginary part
*/
function cimag( a ) {
	return a[ 1 ];
}

/**
* Complex addition: out = a + b.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - first operand
* @param {Float64Array} b - second operand
* @returns {Float64Array} out
*/
function cadd( out, a, b ) {
	out[ 0 ] = a[ 0 ] + b[ 0 ];
	out[ 1 ] = a[ 1 ] + b[ 1 ];
	return out;
}

/**
* Complex subtraction: out = a - b.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - first operand
* @param {Float64Array} b - second operand
* @returns {Float64Array} out
*/
function csub( out, a, b ) {
	out[ 0 ] = a[ 0 ] - b[ 0 ];
	out[ 1 ] = a[ 1 ] - b[ 1 ];
	return out;
}

/**
* Complex multiplication: out = a * b.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - first operand
* @param {Float64Array} b - second operand
* @returns {Float64Array} out
*/
function cmul( out, a, b ) {
	var ar = a[ 0 ];
	var ai = a[ 1 ];
	var br = b[ 0 ];
	var bi = b[ 1 ];
	out[ 0 ] = ar * br - ai * bi;
	out[ 1 ] = ar * bi + ai * br;
	return out;
}

/**
* Complex division: out = a / b.
*
* Uses Smith's formula for numerical stability.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - numerator
* @param {Float64Array} b - denominator
* @returns {Float64Array} out
*/
function cdiv( out, a, b ) {
	var ar = a[ 0 ];
	var ai = a[ 1 ];
	var br = b[ 0 ];
	var bi = b[ 1 ];
	var r;
	var d;
	if ( Math.abs( bi ) <= Math.abs( br ) ) {
		r = bi / br;
		d = br + bi * r;
		out[ 0 ] = ( ar + ai * r ) / d;
		out[ 1 ] = ( ai - ar * r ) / d;
	} else {
		r = br / bi;
		d = bi + br * r;
		out[ 0 ] = ( ar * r + ai ) / d;
		out[ 1 ] = ( ai * r - ar ) / d;
	}
	return out;
}

/**
* Complex conjugate: out = conj(a).
*
* @param {Float64Array} out - output
* @param {Float64Array} a - input
* @returns {Float64Array} out
*/
function cconj( out, a ) {
	out[ 0 ] = a[ 0 ];
	out[ 1 ] = -a[ 1 ];
	return out;
}

/**
* Complex negation: out = -a.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - input
* @returns {Float64Array} out
*/
function cneg( out, a ) {
	out[ 0 ] = -a[ 0 ];
	out[ 1 ] = -a[ 1 ];
	return out;
}

/**
* Scale a complex number by a real scalar: out = s * a.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - complex input
* @param {number} s - real scalar
* @returns {Float64Array} out
*/
function cscale( out, a, s ) {
	out[ 0 ] = s * a[ 0 ];
	out[ 1 ] = s * a[ 1 ];
	return out;
}

/**
* Absolute value (modulus): |a| = sqrt(real^2 + imag^2).
*
* Uses the stable formula to avoid overflow.
*
* @param {Float64Array} a - complex number
* @returns {number} absolute value
*/
function cabs( a ) {
	var ar = Math.abs( a[ 0 ] );
	var ai = Math.abs( a[ 1 ] );
	var mx;
	var mn;
	var r;
	if ( ar === 0.0 && ai === 0.0 ) {
		return 0.0;
	}
	if ( ar >= ai ) {
		mx = ar;
		mn = ai;
	} else {
		mx = ai;
		mn = ar;
	}
	r = mn / mx;
	return mx * Math.sqrt( 1.0 + r * r );
}

/**
* DCABS1: |real(a)| + |imag(a)|.
*
* This is the BLAS convention for a cheap complex "absolute value"
* used in izamax, icamax, etc.
*
* @param {Float64Array} a - complex number
* @returns {number} sum of absolute values of real and imaginary parts
*/
function cabs1( a ) {
	return Math.abs( a[ 0 ] ) + Math.abs( a[ 1 ] );
}

/**
* Copy a complex number: out = a.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - input
* @returns {Float64Array} out
*/
function ccopy( out, a ) {
	out[ 0 ] = a[ 0 ];
	out[ 1 ] = a[ 1 ];
	return out;
}

/**
* Test equality of two complex numbers.
*
* @param {Float64Array} a - first complex number
* @param {Float64Array} b - second complex number
* @returns {boolean} true if equal
*/
function ceq( a, b ) {
	return a[ 0 ] === b[ 0 ] && a[ 1 ] === b[ 1 ];
}

/**
* Test if a complex number is zero.
*
* @param {Float64Array} a - complex number
* @returns {boolean} true if zero
*/
function ciszero( a ) {
	return a[ 0 ] === 0.0 && a[ 1 ] === 0.0;
}

/**
* Multiply-add: out = a + s * b, where s is a real scalar.
*
* Useful for BLAS axpy-style operations on complex elements.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - complex addend
* @param {number} s - real scalar
* @param {Float64Array} b - complex multiplicand
* @returns {Float64Array} out
*/
function cmadd( out, a, s, b ) {
	out[ 0 ] = a[ 0 ] + s * b[ 0 ];
	out[ 1 ] = a[ 1 ] + s * b[ 1 ];
	return out;
}

/**
* Complex multiply-add: out = a + b * c.
*
* @param {Float64Array} out - output
* @param {Float64Array} a - complex addend
* @param {Float64Array} b - first factor
* @param {Float64Array} c - second factor
* @returns {Float64Array} out
*/
function cmmadd( out, a, b, c ) {
	var br = b[ 0 ];
	var bi = b[ 1 ];
	var cr = c[ 0 ];
	var ci = c[ 1 ];
	out[ 0 ] = a[ 0 ] + ( br * cr - bi * ci );
	out[ 1 ] = a[ 1 ] + ( br * ci + bi * cr );
	return out;
}


/**
* Indexed absolute value: |arr[idx] + arr[idx+1]*i|.
*
* Operates directly on a Float64Array at a given index, avoiding subarray
* allocation. Equivalent to cmplx.abs(arr.subarray(idx, idx+2)).
*
* @param {Float64Array} arr - array containing interleaved complex data
* @param {NonNegativeInteger} idx - Float64 index of the real part
* @returns {number} absolute value
*/
function cabsAt( arr, idx ) {
	var ar = Math.abs( arr[ idx ] );
	var ai = Math.abs( arr[ idx + 1 ] );
	var mx;
	var mn;
	var r;
	if ( ar === 0.0 && ai === 0.0 ) {
		return 0.0;
	}
	if ( ar >= ai ) {
		mx = ar;
		mn = ai;
	} else {
		mx = ai;
		mn = ar;
	}
	r = mn / mx;
	return mx * Math.sqrt( 1.0 + r * r );
}

/**
* Indexed DCABS1: |arr[idx]| + |arr[idx+1]|.
*
* @param {Float64Array} arr - array containing interleaved complex data
* @param {NonNegativeInteger} idx - Float64 index of the real part
* @returns {number} sum of absolute values of real and imaginary parts
*/
function cabs1At( arr, idx ) {
	return Math.abs( arr[ idx ] ) + Math.abs( arr[ idx + 1 ] );
}

/**
* Indexed complex multiply: out[oi] = a[ai] * b[bi].
*
* Reads complex numbers from arrays at the given Float64 indices and writes
* the product to out at the given index. All arrays can be the same (aliasing
* is safe as long as out index doesn't overlap inputs).
*
* @param {Float64Array} out - output array
* @param {NonNegativeInteger} oi - Float64 index for output real part
* @param {Float64Array} a - first operand array
* @param {NonNegativeInteger} ai - Float64 index for a's real part
* @param {Float64Array} b - second operand array
* @param {NonNegativeInteger} bi - Float64 index for b's real part
* @returns {Float64Array} out
*/
function cmulAt( out, oi, a, ai, b, bi ) {
	var ar = a[ ai ];
	var aig = a[ ai + 1 ];
	var br = b[ bi ];
	var big = b[ bi + 1 ];
	out[ oi ] = ar * br - aig * big;
	out[ oi + 1 ] = ar * big + aig * br;
	return out;
}

/**
* Indexed complex divide: out[oi] = a[ai] / b[bi].
*
* @param {Float64Array} out - output array
* @param {NonNegativeInteger} oi - Float64 index for output real part
* @param {Float64Array} a - numerator array
* @param {NonNegativeInteger} ai - Float64 index for a's real part
* @param {Float64Array} b - denominator array
* @param {NonNegativeInteger} bi - Float64 index for b's real part
* @returns {Float64Array} out
*/
function cdivAt( out, oi, a, ai, b, bi ) {
	var ar = a[ ai ];
	var aig = a[ ai + 1 ];
	var br = b[ bi ];
	var big = b[ bi + 1 ];
	var r;
	var d;
	if ( Math.abs( big ) <= Math.abs( br ) ) {
		r = big / br;
		d = br + big * r;
		out[ oi ] = ( ar + aig * r ) / d;
		out[ oi + 1 ] = ( aig - ar * r ) / d;
	} else {
		r = br / big;
		d = big + br * r;
		out[ oi ] = ( ar * r + aig ) / d;
		out[ oi + 1 ] = ( aig * r - ar ) / d;
	}
	return out;
}


// EXPORTS //

module.exports = {
	set: cset,
	real: creal,
	imag: cimag,
	add: cadd,
	sub: csub,
	mul: cmul,
	div: cdiv,
	conj: cconj,
	neg: cneg,
	scale: cscale,
	abs: cabs,
	abs1: cabs1,
	copy: ccopy,
	eq: ceq,
	iszero: ciszero,
	madd: cmadd,
	mmadd: cmmadd,
	absAt: cabsAt,
	abs1At: cabs1At,
	mulAt: cmulAt,
	divAt: cdivAt
};
