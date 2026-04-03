

// TypeScript declarations for @stdlib/lapack/base/zgetc2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the LU factorization with complete pivoting of the general n-by-n matrix.
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
* Computes the LU factorization with complete pivoting of the general n-by-n matrix.
*/
declare var zgetc2: Routine;

export = zgetc2;
