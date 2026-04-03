

// TypeScript declarations for @stdlib/lapack/base/dgetf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes an LU factorization of a general M-by-N matrix using partial pivoting with row interchanges (unblocked algorithm).
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
* Computes an LU factorization of a general M-by-N matrix using partial pivoting with row interchanges (unblocked algorithm).
*/
declare var dgetf2: Routine;

export = dgetf2;
