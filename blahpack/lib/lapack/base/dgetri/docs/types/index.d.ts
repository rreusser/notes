

// TypeScript declarations for @stdlib/lapack/base/dgetri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the inverse of a matrix using the LU factorization from dgetrf
	*/
	(
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Compute the inverse of a matrix using the LU factorization from dgetrf
*/
declare var dgetri: Routine;

export = dgetri;
