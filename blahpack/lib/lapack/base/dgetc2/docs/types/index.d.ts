

// TypeScript declarations for @stdlib/lapack/base/dgetc2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* LU factorization with complete pivoting of a general NxN matrix
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
		JPIV: Int32Array,
		strideJPIV: number,
		offsetJPIV: number
	): Float64Array;
}

/**
* LU factorization with complete pivoting of a general NxN matrix
*/
declare var dgetc2: Routine;

export = dgetc2;
