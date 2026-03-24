

// TypeScript declarations for @stdlib/lapack/base/dgttrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the LU factorization of a real tridiagonal matrix
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
* Computes the LU factorization of a real tridiagonal matrix
*/
declare var dgttrf: Routine;

export = dgttrf;
