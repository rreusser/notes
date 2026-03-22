

// TypeScript declarations for @stdlib/lapack/base/dgetrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Blocked LU factorization of a general M-by-N matrix using partial pivoting.
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number
	): Float64Array;
}

/**
* Blocked LU factorization of a general M-by-N matrix using partial pivoting.
*/
declare var dgetrf: Routine;

export = dgetrf;
