

// TypeScript declarations for @stdlib/lapack/base/zgttrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute LU factorization of complex tridiagonal matrix
	*/
	(
		N: number,
		DL: Float64Array,
		strideDL: number,
		offsetDL: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		DU: Float64Array,
		strideDU: number,
		offsetDU: number,
		DU2: Float64Array,
		strideDU2: number,
		offsetDU2: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number
	): Float64Array;
}

/**
* Compute LU factorization of complex tridiagonal matrix
*/
declare var zgttrf: Routine;

export = zgttrf;
